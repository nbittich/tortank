#[derive(Debug, PartialEq)]
pub enum IRI<'a> {
    IRI {
        scheme: &'a str,
        hier_part: IHierPart<'a>,
        query: &'a str,
        fragment: &'a str,
    },
    Reference(RelativeRef<'a>),
    Absolute {
        scheme: &'a str,
        hier_part: IHierPart<'a>,
        query: &'a str,
    },
}

#[derive(Debug, PartialEq)]
pub enum IHierPart<'a> {
    AbEmpty {
        authority: Authority<'a>,
        ipath: IPath<'a>,
    },
    Absolute(IPath<'a>),
    Rootless(IPath<'a>),
    Empty,
}

#[derive(Debug, PartialEq)]
pub enum RelativePart<'a> {
    AbEmpty {
        authority: Authority<'a>,
        ipath: IPath<'a>,
    },
    Absolute(IPath<'a>),
    NoScheme(IPath<'a>),
    Empty(IPath<'a>),
}

#[derive(Debug, PartialEq)]
pub struct RelativeRef<'a> {
    pub relative_part: RelativePart<'a>,
    pub query: &'a str,
    pub fragment: &'a str,
}
#[derive(Debug, PartialEq)]
pub struct Authority<'a> {
    pub user_info: Option<&'a str>,
    pub host: Host<'a>,
    pub port: Option<&'a str>,
}

#[derive(Debug, PartialEq)]
pub enum Host<'a> {
    IPV4(Vec<u8>),
    IPV6(Vec<u16>),
    RegName(Option<&'a str>),
}

#[derive(Debug, PartialEq)]
pub enum IPath<'a> {
    AbEmpty(Vec<&'a str>), // starts with / or is empty
    Absolute {
        snz: &'a str,           // segment non zero (isegment-nz)
        segments: Vec<&'a str>, // isegment
    },
    Rootless {
        snz: &'a str, // isegment-nz
        segments: Vec<&'a str>,
    },

    NoScheme {
        snz_nc: &'a str, // isegment-nz-nc
        segments: Vec<&'a str>,
    },
    Empty, // ipath-empty
}

use nom::{Parser, combinator::complete, error::ParseError};
use nom_language::error::VerboseError;
use parser::{parse_absolute_iri, parse_iri, parse_iri_reference, parse_scheme};

use crate::prelude::alt;

impl<'a> TryFrom<&'a str> for IRI<'a> {
    type Error = nom::Err<VerboseError<&'a str>>;

    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        match alt((
            complete(parse_iri),
            complete(parse_absolute_iri),
            complete(parse_iri_reference),
        ))
        .parse(value)
        {
            Ok((rest, iri)) => {
                if !rest.trim().is_empty() {
                    Err(nom::Err::Error(VerboseError::from_error_kind(
                        value,
                        nom::error::ErrorKind::NonEmpty,
                    )))
                } else {
                    Ok(iri)
                }
            }
            Err(e) => Err(e),
        }
    }
}
impl IRI<'_> {
    pub fn is_relative(&self) -> bool {
        matches!(self, IRI::Reference(_))
    }
    pub fn has_scheme(s: &'_ str) -> bool {
        match parse_scheme(s) {
            Ok((_, scheme)) => !scheme.is_empty(),
            Err(_) => false,
        }
    }
}
pub(crate) mod ip {
    use nom::{
        bytes::complete::take_while_m_n,
        combinator::{success, verify},
        error::ParseError,
        multi::many_m_n,
    };
    use nom_language::error::VerboseError;

    use crate::prelude::*;
    enum Segment {
        Hextet(u16),
        Compressed,
        IpV4(Vec<u8>),
    }
    pub(crate) fn parse_ip_v6(s: &'_ str) -> ParserResult<'_, Vec<u16>> {
        fn hex_to_u16(input: &'_ str) -> Result<u16, std::num::ParseIntError> {
            u16::from_str_radix(input, 16)
        }
        fn recognize_hexadecimal(input: &'_ str) -> ParserResult<'_, &'_ str> {
            recognize(take_while_m_n(1, 4, |c: char| c.is_ascii_hexdigit())).parse(input)
        }
        fn hextet(s: &'_ str) -> ParserResult<'_, u16> {
            map_res(recognize_hexadecimal, hex_to_u16).parse(s)
        }
        fn segment(s: &'_ str) -> ParserResult<'_, Segment> {
            alt((
                map(tag("::"), |_| Segment::Compressed),
                preceded(opt(tag(":")), map(parse_ip_v4, Segment::IpV4)),
                preceded(opt(tag(":")), map(hextet, Segment::Hextet)),
            ))
            .parse(s)
        }
        let mut ipv6: Vec<u16> = Vec::with_capacity(8);
        let (rest, list) = verify(many_m_n(1, 8, segment), |l: &[Segment]| {
            l.iter()
                .filter(|seg| matches!(seg, Segment::Compressed))
                .count()
                <= 1
                && l.iter()
                    .filter(|seg| matches!(seg, Segment::IpV4(_)))
                    .count()
                    <= 1
        })
        .parse(s)?;

        let mut compression_pos = None;
        for (idx, segment) in list.into_iter().enumerate() {
            match segment {
                Segment::Hextet(v) => ipv6.push(v),
                Segment::Compressed => {
                    compression_pos = Some(idx);
                }
                Segment::IpV4(_) if idx == 0 => {
                    let err = VerboseError::from_error_kind(s, ErrorKind::Verify);
                    return Err(nom::Err::Error(err));
                }
                Segment::IpV4(l) => {
                    ipv6.push((l[0] as u16) << 8 | l[1] as u16);
                    ipv6.push((l[2] as u16) << 8 | l[3] as u16);
                }
            }
        }
        if let Some(idx) = compression_pos {
            while ipv6.len() < 8 {
                ipv6.insert(idx, 0x0);
            }
        }
        let (_, ipv6) = verify(success(ipv6), |l: &[u16]| l.len() == 8).parse("")?;

        Ok((rest, ipv6))
    }
    pub(crate) fn parse_ip_v4(s: &'_ str) -> ParserResult<'_, Vec<u8>> {
        verify(
            separated_list1(
                tag("."),
                map_parser(take_while1(|c: char| c.is_numeric()), all_consuming(U8)),
            ),
            |list: &[u8]| list.len() == 4,
        )
        .parse(s)
    }
}

mod parser {
    use nom::{
        bytes::streaming::take_while1,
        multi::{many0_count, many1_count},
    };

    use crate::prelude::*;

    use super::{
        Authority, Host, IHierPart, IPath, IRI, RelativePart, RelativeRef,
        ip::{parse_ip_v4, parse_ip_v6},
    };

    fn parse_i_query(s: &'_ str) -> ParserResult<'_, &'_ str> {
        let (rest, _) =
            many0_count(alt((parse_ip_char, parse_i_private, tag("/"), tag("?")))).parse(s)?;
        let value = &s[0..s.len() - rest.len()];
        Ok((rest, value))
    }
    fn parse_authority(s: &'_ str) -> ParserResult<'_, Authority<'_>> {
        map(
            (
                opt(terminated(parse_userinfo, tag("@"))),
                parse_host,
                opt(preceded(tag(":"), parse_port)),
            ),
            |(user_info, host, port)| Authority {
                user_info,
                host,
                port,
            },
        )
        .parse(s)
    }

    pub(super) fn parse_iri_reference(s: &'_ str) -> ParserResult<'_, IRI<'_>> {
        map(parse_i_relative_ref, IRI::Reference).parse(s)
    }
    pub(super) fn parse_iri(s: &'_ str) -> ParserResult<'_, IRI<'_>> {
        map(
            (
                parse_scheme,
                parse_i_hier_part,
                preceded(opt(tag("?")), parse_i_query),
                preceded(opt(tag("#")), parse_i_fragment),
            ),
            |(scheme, hier_part, query, fragment)| IRI::IRI {
                scheme,
                hier_part,
                query,
                fragment,
            },
        )
        .parse(s)
    }
    pub(super) fn parse_absolute_iri(s: &'_ str) -> ParserResult<'_, IRI<'_>> {
        map(
            (
                parse_scheme,
                parse_i_hier_part,
                preceded(opt(tag("?")), parse_i_query),
            ),
            |(scheme, hier_part, query)| IRI::Absolute {
                scheme,
                hier_part,
                query,
            },
        )
        .parse(s)
    }
    fn parse_i_relative_ref(s: &'_ str) -> ParserResult<'_, RelativeRef<'_>> {
        map(
            (
                parse_i_relative_part,
                preceded(opt(tag("?")), parse_i_query),
                preceded(opt(tag("#")), parse_i_fragment),
            ),
            |(relative_part, query, fragment)| RelativeRef {
                relative_part,
                query,
                fragment,
            },
        )
        .parse(s)
    }
    fn parse_i_relative_part(s: &'_ str) -> ParserResult<'_, RelativePart<'_>> {
        alt((
            map(
                preceded(tag("//"), pair(parse_authority, parse_ipath_abempty)),
                |(authority, ipath)| RelativePart::AbEmpty { authority, ipath },
            ),
            map(parse_ipath_absolute, RelativePart::Absolute),
            map(parse_ipath_noscheme, RelativePart::NoScheme),
            map(parse_ipath_empty, RelativePart::Empty),
        ))
        .parse(s)
    }
    fn parse_i_hier_part(s: &'_ str) -> ParserResult<'_, IHierPart<'_>> {
        alt((
            map(
                preceded(tag("//"), pair(parse_authority, parse_ipath_abempty)),
                |(authority, ipath)| IHierPart::AbEmpty { authority, ipath },
            ),
            map(parse_ipath_absolute, IHierPart::Absolute),
            map(parse_ipath_rootless, IHierPart::Rootless),
            map(parse_ipath_empty, |_| IHierPart::Empty),
        ))
        .parse(s)
    }
    fn parse_host(s: &'_ str) -> ParserResult<'_, Host<'_>> {
        alt((
            map(
                preceded(opt(tag("[")), terminated(parse_ip_v6, opt(tag("]")))),
                Host::IPV6,
            ),
            map(parse_ip_v4, Host::IPV4),
            map(opt(parse_i_reg_name), Host::RegName),
        ))
        .parse(s)
    }

    fn parse_i_fragment(s: &'_ str) -> ParserResult<'_, &'_ str> {
        let (rest, _) = many0_count(alt((parse_ip_char, tag("/"), tag("?")))).parse(s)?;
        let value = &s[0..s.len() - rest.len()];
        Ok((rest, value))
    }
    fn parse_ipath_empty(s: &'_ str) -> ParserResult<'_, IPath<'_>> {
        map(
            verify(peek(opt(parse_ip_char)), |ip_char| ip_char.is_none()),
            |_| IPath::Empty,
        )
        .parse(s)
    }

    fn parse_ipath_rootless(s: &'_ str) -> ParserResult<'_, IPath<'_>> {
        map(
            pair(
                parse_i_segmentnz,
                many0(recognize(preceded(tag("/"), parse_i_segmentnz))),
            ),
            |(snz, segments)| IPath::Rootless { snz, segments },
        )
        .parse(s)
    }

    fn parse_ipath_noscheme(s: &'_ str) -> ParserResult<'_, IPath<'_>> {
        map(
            pair(
                parse_i_segmentnz_nc,
                many0(recognize(preceded(tag("/"), parse_i_segment0))),
            ),
            |(snz_nc, segments)| IPath::NoScheme { snz_nc, segments },
        )
        .parse(s)
    }
    fn parse_ipath_abempty(s: &'_ str) -> ParserResult<'_, IPath<'_>> {
        map(
            many0(recognize(preceded(tag("/"), parse_i_segment0))),
            IPath::AbEmpty,
        )
        .parse(s)
    }
    fn parse_ipath_absolute(s: &'_ str) -> ParserResult<'_, IPath<'_>> {
        let (first_two, _) = peek(take(2usize)).parse(s)?;
        let parser = pair(
            parse_i_segmentnz,
            many0(recognize(preceded(tag("/"), parse_i_segment0))),
        );
        verify(
            map(parser, |(snz, segments)| IPath::Absolute { snz, segments }),
            move |_| first_two.starts_with("/") && first_two != "//",
        )
        .parse(s)
    }
    fn parse_i_segmentnz(s: &'_ str) -> ParserResult<'_, &'_ str> {
        let (rest, _) = many0_count(parse_ip_char).parse(s)?;
        let value = &s[0..s.len() - rest.len()];
        Ok((rest, value))
    }
    fn parse_i_segment0(s: &'_ str) -> ParserResult<'_, &'_ str> {
        let (rest, _) = many0_count(parse_ip_char).parse(s)?;
        let value = &s[0..s.len() - rest.len()];
        Ok((rest, value))
    }

    fn parse_i_segmentnz_nc(s: &'_ str) -> ParserResult<'_, &'_ str> {
        let (rest, _) = many1_count(alt((
            parse_i_unreserved,
            parse_pct_encoded,
            parse_sub_delims,
            tag("@"),
        )))
        .parse(s)?;
        let value = &s[0..s.len() - rest.len()];
        Ok((rest, value))
    }
    fn parse_ip_char(s: &'_ str) -> ParserResult<'_, &'_ str> {
        alt((
            parse_i_unreserved,
            parse_pct_encoded,
            parse_sub_delims,
            tag(":"),
            tag("@"),
        ))
        .parse(s)
    }
    pub(super) fn parse_scheme(s: &'_ str) -> ParserResult<'_, &'_ str> {
        terminated(
            verify(
                take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-' || c == '+'),
                |scheme: &'_ str| scheme.starts_with(|c: char| c.is_alphabetic()),
            ),
            tag(":"),
        )
        .parse(s)
    }
    fn parse_userinfo(s: &'_ str) -> ParserResult<'_, &'_ str> {
        let (rest, _) = many1_count(alt((
            parse_pct_encoded,
            parse_i_unreserved,
            parse_sub_delims,
            tag(":"),
        )))
        .parse(s)?;
        let value = &s[0..s.len() - rest.len()];
        Ok((rest, value))
    }
    fn parse_port(s: &'_ str) -> ParserResult<'_, &'_ str> {
        take_while1(|p: char| p.is_numeric())(s)
    }
    fn parse_i_reg_name(s: &'_ str) -> ParserResult<'_, &'_ str> {
        let (rest, _) = many1_count(alt((
            parse_pct_encoded,
            parse_i_unreserved,
            parse_sub_delims,
        )))
        .parse(s)?;
        let value = &s[0..s.len() - rest.len()];
        Ok((rest, value))
    }
    fn parse_i_private(s: &'_ str) -> ParserResult<'_, &'_ str> {
        verify(take(1usize), |hex: &'_ str| {
            hex.starts_with(|c: char| {
                matches!(
                    c,
                    '\u{E000}'..='\u{F8FF}'
                    | '\u{F0000}'..='\u{FFFFD}'
                    | '\u{100000}'..='\u{10FFFD}'
                )
            })
        })
        .parse(s)
    }
    fn parse_i_unreserved(s: &'_ str) -> ParserResult<'_, &'_ str> {
        fn is_ucs_char(c: &char) -> bool {
            matches!(c,
                '\u{00A0}'..='\u{D7FF}' |
                '\u{F900}'..='\u{FDCF}' |
                '\u{FDF0}'..='\u{FFEF}' |
                '\u{10000}'..='\u{1FFFD}' |
                '\u{20000}'..='\u{2FFFD}' |
                '\u{30000}'..='\u{3FFFD}' |
                '\u{40000}'..='\u{4FFFD}' |
                '\u{50000}'..='\u{5FFFD}' |
                '\u{60000}'..='\u{6FFFD}' |
                '\u{70000}'..='\u{7FFFD}' |
                '\u{80000}'..='\u{8FFFD}' |
                '\u{90000}'..='\u{9FFFD}' |
                '\u{A0000}'..='\u{AFFFD}' |
                '\u{B0000}'..='\u{BFFFD}' |
                '\u{C0000}'..='\u{CFFFD}' |
                '\u{D0000}'..='\u{DFFFD}' |
                '\u{E1000}'..='\u{EFFFD}'
            )
        }
        verify(take(1usize), |unres: &'_ str| {
            unres.starts_with(|c: char| {
                c.is_alphanum() || c == '-' || c == '.' || c == '_' || c == '~' || is_ucs_char(&c)
            })
        })
        .parse(s)
    }
    fn parse_sub_delims(s: &'_ str) -> ParserResult<'_, &'_ str> {
        verify(take(1usize), |c: &'_ str| {
            c == "!"
                || c == "$"
                || c == "&"
                || c == "'"
                || c == "("
                || c == ")"
                || c == "*"
                || c == "+"
                || c == ","
                || c == ";"
                || c == "="
        })
        .parse(s)
    }
    fn parse_pct_encoded(s: &'_ str) -> ParserResult<'_, &'_ str> {
        recognize(preceded(
            tag("%"),
            verify(take(2usize), |hex: &'_ str| {
                hex.chars().all(|c| c.is_ascii_hexdigit())
            }),
        ))
        .parse(s)
    }
    fn _hex_to_char(hex: &'_ str) -> Option<char> {
        u32::from_str_radix(hex, 16).ok().and_then(char::from_u32)
    }
}
