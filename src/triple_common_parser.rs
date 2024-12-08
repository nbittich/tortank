use std::borrow::Cow;

use crate::prelude::{
    char, delimited, line_ending, many0, multispace0, preceded, tag, tag_no_case, take_until,
    ParserResult,
};
pub static BASE_TURTLE: &str = "@base";
pub static BASE_SPARQL: &str = "BASE";
pub static PREFIX_TURTLE: &str = "@prefix";
pub static PREFIX_SPARQL: &str = "PREFIX";

#[derive(PartialEq, Debug)]
pub enum Iri<'a> {
    Enclosed(&'a str),
    Prefixed {
        prefix: &'a str,
        local_name: &'a str,
    },
}
#[derive(PartialEq, Debug)]
pub enum Literal<'a> {
    Quoted {
        datatype: Option<Iri<'a>>,
        value: Cow<'a, str>,
        lang: Option<&'a str>,
    },
    Double(f64),
    Decimal(f32),
    Integer(i64),
    Boolean(bool),
}
#[derive(PartialEq, Debug)]
pub enum BlankNode<'a> {
    Labeled(&'a str),
    Unlabeled,
}
pub(crate) mod iri {
    use crate::grammar::PN_LOCAL_ESC;
    use crate::prelude::*;
    use crate::triple_common_parser::Iri;
    use nom::bytes::complete::escaped;
    use nom::character::complete::one_of;
    pub(crate) fn prefixed_iri(s: &str) -> ParserResult<Iri> {
        let prefixed = map(
            separated_pair(
                take_while(|s: char| s.is_alphanumeric()),
                tag(":"),
                alt((
                    take_while(|s: char| s.is_alphanumeric() || PN_LOCAL_ESC.contains(s)),
                    escaped(alphanumeric1, '\\', one_of(PN_LOCAL_ESC)),
                )),
            ),
            |(prefix, local_name)| Iri::Prefixed { prefix, local_name },
        );
        preceded(multispace0, prefixed)(s)
    }
    pub(crate) fn iri(s: &str) -> ParserResult<Iri> {
        alt((prefixed_iri, enclosed_iri))(s)
    }
    pub(crate) fn enclosed_iri(s: &str) -> ParserResult<Iri> {
        let enclosed = map(
            delimited(char('<'), take_while(|s: char| s != '>'), char('>')),
            Iri::Enclosed,
        );

        preceded(multispace0, enclosed)(s)
    }
}

pub(crate) mod prologue {
    use super::iri::enclosed_iri;
    use crate::prelude::*;
    use crate::triple_common_parser::{
        Iri, BASE_SPARQL, BASE_TURTLE, PREFIX_SPARQL, PREFIX_TURTLE,
    };

    pub(crate) fn base_sparql(s: &str) -> ParserResult<Iri> {
        base(BASE_SPARQL, enclosed_iri)(s)
    }
    pub(crate) fn base_turtle(s: &str) -> ParserResult<Iri> {
        base(
            BASE_TURTLE,
            terminated(enclosed_iri, preceded(multispace0, char('.'))),
        )(s)
    }
    fn base<'a, F>(
        base_tag: &'static str,
        extract_base: F,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, Iri<'a>>
    where
        F: FnMut(&'a str) -> ParserResult<'a, Iri<'a>>,
    {
        preceded(preceded(multispace0, tag_no_case(base_tag)), extract_base)
    }
    fn prefix<'a>(
        prefix_tag: &'static str,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, (&'a str, Iri<'a>)> {
        preceded(
            preceded(multispace0, tag_no_case(prefix_tag)),
            separated_pair(
                preceded(multispace0, take_until(":")),
                tag(":"),
                enclosed_iri,
            ),
        )
    }
    pub(crate) fn prefix_turtle(s: &str) -> ParserResult<(&str, Iri<'_>)> {
        terminated(prefix(PREFIX_TURTLE), preceded(multispace0, char('.')))(s)
    }
    pub(crate) fn prefix_sparql(s: &str) -> ParserResult<(&str, Iri<'_>)> {
        prefix(PREFIX_SPARQL)(s)
    }
}
pub(crate) mod literal {
    use std::borrow::Cow;

    use crate::grammar::{
        LANGTAG, STRING_LITERAL_LONG_QUOTE, STRING_LITERAL_LONG_SINGLE_QUOTE, STRING_LITERAL_QUOTE,
        STRING_LITERAL_SINGLE_QUOTE,
    };
    use crate::prelude::*;
    use crate::shared::XSD_STRING;
    use crate::string_parser::parse_escaped_string;
    use crate::triple_common_parser::iri::iri;
    use crate::triple_common_parser::{tag_no_space, Iri, Literal};
    pub(crate) fn parse_boolean<'a>(
        case_sensitive: bool,
    ) -> impl FnMut(&'a str) -> ParserResult<Literal<'a>> {
        move |s| {
            let extractor = |s| {
                if case_sensitive {
                    alt((tag("true"), tag("false")))(s)
                } else {
                    alt((tag_no_case("true"), tag_no_case("false")))(s)
                }
            };
            map(
                terminated(
                    map(extractor, |bv: &'a str| bv.eq_ignore_ascii_case("true")),
                    multispace0,
                ),
                Literal::Boolean,
            )(s)
        }
    }

    pub(crate) fn parse_number(s: &str) -> ParserResult<Literal> {
        map_parser(
            recognize_float,
            alt((
                map(all_consuming(I64), Literal::Integer),
                map(all_consuming(float), Literal::Decimal),
                map(all_consuming(double), Literal::Double),
            )),
        )(s)
    }

    #[allow(unused)]
    pub(crate) fn primitive_literal_sparql(s: &str) -> ParserResult<Literal> {
        preceded(multispace0, alt((parse_boolean(false), parse_number)))(s)
    }
    pub(crate) fn primitive_literal_turtle(s: &str) -> ParserResult<Literal> {
        preceded(multispace0, alt((parse_boolean(true), parse_number)))(s)
    }

    pub(crate) fn string_literal(s: &str) -> ParserResult<Literal> {
        let long_single_quote_literal = delimited(
            tag(STRING_LITERAL_LONG_SINGLE_QUOTE),
            take_until(STRING_LITERAL_LONG_SINGLE_QUOTE),
            tag(STRING_LITERAL_LONG_SINGLE_QUOTE),
        );
        let long_quote_literal = delimited(
            tag(STRING_LITERAL_LONG_QUOTE),
            take_until(STRING_LITERAL_LONG_QUOTE),
            tag(STRING_LITERAL_LONG_QUOTE),
        );

        let mut datatype = preceded(tag("^^"), iri);

        fn lang(s: &str) -> ParserResult<&str> {
            preceded(tag(LANGTAG), take_while(|a: char| a.is_alpha() || a == '-'))(s)
        }

        let (remaining, string_literal) = preceded(
            multispace0,
            alt((
                map(
                    alt((long_quote_literal, long_single_quote_literal)),
                    Cow::Borrowed,
                ),
                alt((
                    delimited(
                        tag(STRING_LITERAL_SINGLE_QUOTE),
                        parse_escaped_string,
                        tag(STRING_LITERAL_SINGLE_QUOTE),
                    ),
                    delimited(
                        tag(STRING_LITERAL_QUOTE),
                        parse_escaped_string,
                        tag(STRING_LITERAL_QUOTE),
                    ),
                )),
                map(
                    delimited(
                        tag_no_space(STRING_LITERAL_QUOTE),
                        take_until(STRING_LITERAL_QUOTE),
                        tag_no_space(STRING_LITERAL_QUOTE),
                    ),
                    Cow::Borrowed,
                ),
                map(
                    delimited(
                        tag_no_space(STRING_LITERAL_SINGLE_QUOTE),
                        take_until(STRING_LITERAL_SINGLE_QUOTE),
                        tag_no_space(STRING_LITERAL_SINGLE_QUOTE),
                    ),
                    Cow::Borrowed,
                ),
            )),
        )(s)?;

        if let Ok((remaining, datatype)) = datatype(remaining) {
            Ok((
                remaining,
                Literal::Quoted {
                    datatype: Some(datatype),
                    value: string_literal,
                    lang: None,
                },
            ))
        } else if let Ok((remaining, lang)) = lang(remaining) {
            Ok((
                remaining,
                Literal::Quoted {
                    datatype: None,
                    value: string_literal,
                    lang: Some(lang),
                },
            ))
        } else {
            Ok((
                remaining,
                Literal::Quoted {
                    datatype: Some(Iri::Enclosed(XSD_STRING)),
                    value: string_literal,
                    lang: None,
                },
            ))
        }
    }

    pub(crate) fn literal_turtle(s: &str) -> ParserResult<Literal> {
        alt((string_literal, primitive_literal_turtle))(s)
    }

    #[allow(unused)]
    pub(crate) fn literal_sparql(s: &str) -> ParserResult<Literal> {
        alt((string_literal, primitive_literal_sparql))(s)
    }
}
pub(crate) mod triple {

    use nom::error::{ParseError, VerboseError};

    use crate::grammar::BLANK_NODE_LABEL;
    use crate::prelude::*;
    use crate::shared::NS_TYPE;
    use crate::triple_common_parser::{comments, paren_close, paren_open, BlankNode, Iri};

    use std::collections::VecDeque;

    pub(crate) fn object_list<'a, F1, F2, T>(
        object_extractor: F1,
        mut map_list: F2,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, T>
    where
        F1: FnMut(&'a str) -> ParserResult<'a, T>,
        F2: FnMut(Vec<T>) -> T,
    {
        map(
            separated_list1(
                preceded(multispace0, terminated(char(','), multispace0)),
                object_extractor,
            ),
            move |mut list| {
                if list.len() > 1 {
                    map_list(list)
                } else {
                    list.pop().unwrap()
                }
            },
        )
    }
    pub(crate) fn ns_type(s: &str) -> ParserResult<Iri> {
        map(
            preceded(multispace0, terminated(char('a'), multispace1)),
            |_| Iri::Enclosed(NS_TYPE),
        )(s)
    }
    pub(crate) fn predicate_list<'a, F1, F2, F3, F4, F5, T>(
        subject_extractor: F1,
        predicate_extractor: F2,
        object_list_extractor: F3,
        map_predicate_object: F4,
        map_statement: F5,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, T>
    where
        F1: Fn(&'a str) -> ParserResult<'a, T>,
        F2: Fn(&'a str) -> ParserResult<'a, T>,
        F3: Fn(&'a str) -> ParserResult<'a, T>,
        F4: Fn((T, T)) -> T,
        F5: Fn(T, Vec<T>) -> T,
    {
        map(
            pair(
                subject_extractor,
                preceded(
                    multispace0,
                    separated_list1(
                        delimited(multispace0, tag(";"), comments),
                        map(
                            pair(predicate_extractor, object_list_extractor),
                            map_predicate_object,
                        ),
                    ),
                ),
            ),
            move |(subject, list)| map_statement(subject, list),
        )
    }
    pub(crate) fn collection<'a, T, F>(
        object_extractor: F,
    ) -> impl FnMut(&'a str) -> ParserResult<'a, VecDeque<T>>
    where
        F: Fn(&'a str) -> ParserResult<'a, T>,
    {
        map(
            preceded(
                paren_open,
                terminated(
                    separated_list0(multispace1, object_extractor),
                    preceded(multispace0, cut(paren_close)),
                ),
            ),
            VecDeque::from,
        )
    }
    pub(crate) fn anon_bnode<'a, F, T>(anon_parser: F) -> impl FnMut(&'a str) -> ParserResult<'a, T>
    where
        F: FnMut(&'a str) -> ParserResult<'a, T>,
    {
        preceded(
            multispace0,
            preceded(
                char('['),
                terminated(anon_parser, preceded(multispace0, cut(char(']')))),
            ),
        )
    }
    // https://www.w3.org/TR/turtle/ 2.6 RDF Blank Nodes
    pub(crate) fn labeled_bnode(s: &str) -> ParserResult<BlankNode> {
        fn allowed_but_not_as_first(c: char) -> bool {
            matches!(c,'.' | '-' | '·' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}')
        }
        let (s, _) = preceded(multispace0, tag(BLANK_NODE_LABEL))(s)?;
        let mut idx_bnode = 0;
        for c in s.chars() {
            if c.is_alphanum() || c == '_' || allowed_but_not_as_first(c) {
                idx_bnode += c.len_utf8();
            } else {
                break;
            }
        }
        let mut bnode: &str = &s[0..idx_bnode];
        if bnode.ends_with('.') {
            idx_bnode -= '.'.len_utf8();
            bnode = &s[0..idx_bnode];
        }
        if bnode.is_empty()
            || bnode
                .chars()
                .last()
                .filter(|c| allowed_but_not_as_first(*c))
                .is_some()
            || bnode.chars().take(1).any(allowed_but_not_as_first)
        {
            let err = VerboseError::from_error_kind(s, ErrorKind::IsNot);
            return Err(nom::Err::Error(err));
        }
        let rest = &s[idx_bnode..];
        Ok((rest, BlankNode::Labeled(bnode)))
    }
}
pub(crate) fn comments(s: &str) -> ParserResult<Vec<&str>> {
    many0(delimited(
        multispace0,
        preceded(char('#'), take_until("\n")),
        line_ending,
    ))(s)
}

pub(crate) fn paren_close(s: &str) -> ParserResult<&str> {
    preceded(multispace0, tag(")"))(s)
}
pub(crate) fn paren_open(s: &str) -> ParserResult<&str> {
    tag_no_space("(")(s)
}

pub(crate) fn tag_no_space<'a>(s: &'a str) -> impl FnMut(&'a str) -> ParserResult<&'a str> {
    delimited(multispace0, tag(s), multispace0)
}
#[allow(unused)]
pub(crate) fn tag_no_case_no_space<'a>(s: &'a str) -> impl FnMut(&'a str) -> ParserResult<&'a str> {
    delimited(multispace0, tag_no_case(s), multispace0)
}
