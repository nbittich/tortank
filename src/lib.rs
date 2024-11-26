pub mod iri;
mod shared;
mod string_parser;
mod triple_common_parser;
pub mod turtle;
pub mod prelude {
    use nom::error::VerboseError;
    pub use nom::{
        branch::alt,
        bytes::complete::{
            tag, tag_no_case, take, take_till, take_till1, take_until, take_until1, take_while,
            take_while1,
        },
        character::{
            complete::{
                alphanumeric1, char, i64 as I64, line_ending, multispace0, multispace1, space0,
                space1, u16 as U16, u32 as U32, u8 as U8,
            },
            is_alphanumeric, is_space,
        },
        combinator::{
            all_consuming, cut, eof, map, map_parser, map_res, opt, peek, recognize, value, verify,
        },
        error::{make_error, Error, ErrorKind},
        multi::{many0, separated_list0, separated_list1},
        number::complete::{double, float, recognize_float},
        sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
        AsChar, IResult, InputIter, ParseTo, Parser,
    };
    pub type ParserResult<'a, T> = IResult<&'a str, T, VerboseError<&'a str>>;
}

pub mod grammar {
    pub const PN_LOCAL_ESC: &str = "_~-!$&\\:()*+=/?#%";
    pub const PERCENT: &str = "%";
    pub const STRING_LITERAL_QUOTE: &str = r#"""#;
    pub const STRING_LITERAL_SINGLE_QUOTE: &str = "'";
    pub const STRING_LITERAL_LONG_SINGLE_QUOTE: &str = "'''";
    pub const STRING_LITERAL_LONG_QUOTE: &str = r#"""""#;
    pub const LANGTAG: &str = "@";
    pub const BLANK_NODE_LABEL: &str = "_:";
}

#[cfg(test)]
mod tests;
