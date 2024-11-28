pub struct IRI {
    pub scheme: Option<String>,
    pub i_hier_part: Option<IHierPart>,
}

pub struct IHierPart {
    pub authority: Option<Authority>,
}

pub struct Authority {
    pub user_info: Option<String>,
    pub host: Host,
    pub port: Option<String>,
}

pub enum Host {
    IPV4(Vec<u8>),
    IPV6(Vec<u16>),
    RegName(Option<String>),
}

pub enum IPath {
    AbEmpty(Vec<String>), // starts with / or is empty
    AbAbsolute {
        snz: String,           // segment non zero (isegment-nz)
        segments: Vec<String>, // isegment
    },
    Rootless {
        snz_nc: String, // isegment-nz-nc
        segments: Vec<String>,
    },
    Empty, // ipath-empty
}

#[allow(unused)]
mod ip {
    use nom::{
        bytes::complete::take_while_m_n,
        combinator::{success, verify},
        error::{ParseError, VerboseError},
        multi::many_m_n,
    };

    use crate::prelude::*;
    enum Segment {
        Hextet(u16),
        Compressed,
        IpV4(Vec<u8>),
    }
    pub(super) fn parse_ip_v6(s: &str) -> ParserResult<Vec<u16>> {
        fn hex_to_u16(input: &str) -> Result<u16, std::num::ParseIntError> {
            u16::from_str_radix(input, 16)
        }
        fn recognize_hexadecimal(input: &str) -> ParserResult<&str> {
            recognize(take_while_m_n(1, 4, |c: char| c.is_ascii_hexdigit()))(input)
        }
        fn hextet(s: &str) -> ParserResult<u16> {
            map_res(recognize_hexadecimal, hex_to_u16)(s)
        }
        fn segment(s: &str) -> ParserResult<Segment> {
            alt((
                map(tag("::"), |_| Segment::Compressed),
                preceded(opt(tag(":")), map(parse_ip_v4, Segment::IpV4)),
                preceded(opt(tag(":")), map(hextet, Segment::Hextet)),
            ))(s)
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
        })(s)?;

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
        let (_, ipv6) = verify(success(ipv6), |l: &[u16]| l.len() == 8)("")?;

        Ok((rest, ipv6))
    }
    pub(super) fn parse_ip_v4(s: &str) -> ParserResult<Vec<u8>> {
        verify(
            separated_list1(
                tag("."),
                map_parser(take_while1(|c: char| c.is_numeric()), all_consuming(U8)),
            ),
            |list: &[u8]| list.len() == 4,
        )(s)
    }
}

#[allow(unused)]
mod parser {
    use nom::{
        bytes::streaming::take_while1,
        character::complete::anychar,
        error::{ParseError, VerboseError},
        multi::{fold_many0, fold_many1, many1},
    };

    use crate::prelude::*;

    use super::{
        ip::{self, parse_ip_v4, parse_ip_v6},
        Authority, Host, IPath,
    };

    fn parse_authority(s: &str) -> ParserResult<Authority> {
        map(
            tuple((
                opt(parse_userinfo),
                parse_host,
                opt(preceded(tag(":"), parse_port)),
            )),
            |(user_info, host, port)| Authority {
                user_info,
                host,
                port: port.map(String::from),
            },
        )(s)
    }

    fn parse_host(s: &str) -> ParserResult<Host> {
        alt((
            map(parse_ip_v4, Host::IPV4),
            map(parse_ip_v6, Host::IPV6),
            map(opt(parse_i_reg_name), Host::RegName),
        ))(s)
    }

    fn parse_i_fragment(s: &str) -> ParserResult<String> {
        fold_many0(
            alt((parse_ip_char, tag("/"), tag("?"))),
            String::new,
            |mut acc, item| {
                acc.push_str(item);
                acc
            },
        )(s)
    }
    fn parse_ipath_empty(s: &str) -> ParserResult<IPath> {
        map(
            verify(peek(opt(parse_ip_char)), |ip_char| ip_char.is_none()),
            |_| IPath::Empty,
        )(s)
    }

    fn parse_ipath_rootless(s: &str) -> ParserResult<IPath> {
        map(
            pair(
                parse_i_segmentnz_nc,
                many0(preceded(tag("/"), parse_i_segment0)),
            ),
            |(snz_nc, segments)| IPath::Rootless { snz_nc, segments },
        )(s)
    }

    fn parse_ipath_abempty(s: &str) -> ParserResult<IPath> {
        map(many0(preceded(tag("/"), parse_i_segment0)), IPath::AbEmpty)(s)
    }
    fn parse_ipath_absolute(s: &str) -> ParserResult<IPath> {
        let (first_two, _) = peek(take(2usize))(s)?;
        let parser = pair(
            parse_i_segmentnz,
            many0(preceded(tag("/"), parse_i_segment0)),
        );
        verify(
            map(parser, |(snz, segments)| IPath::AbAbsolute {
                snz,
                segments,
            }),
            move |_| first_two.starts_with("/") && first_two != "//",
        )(s)
    }
    fn parse_i_segmentnz(s: &str) -> ParserResult<String> {
        fold_many0(parse_ip_char, String::new, |mut acc, item| {
            acc.push_str(item);
            acc
        })(s)
    }
    fn parse_i_segment0(s: &str) -> ParserResult<String> {
        fold_many0(parse_ip_char, String::new, |mut acc, item| {
            acc.push_str(item);
            acc
        })(s)
    }

    fn parse_i_segmentnz_nc(s: &str) -> ParserResult<String> {
        fold_many1(
            alt((
                parse_i_unreserved,
                parse_pct_encoded,
                parse_sub_delims,
                tag("@"),
            )),
            String::new,
            |mut acc, item| {
                acc.push_str(item);
                acc
            },
        )(s)
    }
    fn parse_ip_char(s: &str) -> ParserResult<&str> {
        alt((
            parse_i_unreserved,
            parse_pct_encoded,
            parse_sub_delims,
            tag(":"),
            tag("@"),
        ))(s)
    }
    fn parse_scheme(s: &str) -> ParserResult<&str> {
        verify(
            take_while1(|c: char| c.is_alphanumeric() || c == '.' || c == '-' || c == '+'),
            |scheme: &str| scheme.starts_with(|c: char| c.is_alphabetic()),
        )(s)
    }
    fn parse_userinfo(s: &str) -> ParserResult<String> {
        fold_many1(
            alt((
                parse_pct_encoded,
                parse_i_unreserved,
                parse_sub_delims,
                tag(":"),
            )),
            String::new,
            |mut acc: String, item| {
                acc.push_str(item);
                acc
            },
        )(s)
    }
    fn parse_port(s: &str) -> ParserResult<&str> {
        take_while1(|p: char| p.is_numeric())(s)
    }
    fn parse_i_reg_name(s: &str) -> ParserResult<String> {
        fold_many1(
            alt((parse_pct_encoded, parse_i_unreserved, parse_sub_delims)),
            String::new,
            |mut acc: String, item| {
                acc.push_str(item);
                acc
            },
        )(s)
    }
    fn parse_i_private(s: &str) -> ParserResult<&str> {
        verify(take(1usize), |hex: &str| {
            hex.starts_with(|c: char| {
                matches!(
                    c,
                    '\u{E000}'..='\u{F8FF}'
                    | '\u{F0000}'..='\u{FFFFD}'
                    | '\u{100000}'..='\u{10FFFD}'
                )
            })
        })(s)
    }
    fn parse_i_unreserved(s: &str) -> ParserResult<&str> {
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
        verify(take(1usize), |unres: &str| {
            unres.starts_with(|c: char| {
                c.is_alphanum() || c == '-' || c == '.' || c == '_' || c == '~' || is_ucs_char(&c)
            })
        })(s)
    }
    fn parse_sub_delims(s: &str) -> ParserResult<&str> {
        verify(take(1usize), |c: &str| {
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
        })(s)
    }
    fn parse_pct_encoded(s: &str) -> ParserResult<&str> {
        preceded(
            tag("%"),
            verify(take(2usize), |hex: &str| {
                hex.chars().all(|c| c.is_ascii_hexdigit())
            }),
        )(s)
    }
    fn hex_to_char(hex: &str) -> Option<char> {
        u32::from_str_radix(hex, 16)
            .ok()
            .and_then(|u| char::from_u32(u))
    }
}

#[cfg(test)]
mod test {

    use crate::iri::ip::{parse_ip_v4, parse_ip_v6};

    #[test]
    fn test_hex_st_to_char() {
        println!("{}", u8::from_str_radix("3A", 16).unwrap() as char);
    }
    #[test]
    fn parse_ip_v4_test() {
        assert_eq!(
            parse_ip_v4("192.168.0.1").unwrap(),
            ("", [192, 168, 0, 1].to_vec())
        );
        assert_eq!(
            parse_ip_v4("127.0.0.1").unwrap(),
            ("", [127, 0, 0, 1].to_vec())
        );
        assert_eq!(parse_ip_v4("8.8.8.8").unwrap(), ("", [8, 8, 8, 8].to_vec()));
        assert_eq!(
            parse_ip_v4("255.255.255.255").unwrap(),
            ("", [255, 255, 255, 255].to_vec())
        );
        assert!(parse_ip_v4("256.1.1.1").is_err());
        assert!(parse_ip_v4("192.168.0").is_err());
        assert!(parse_ip_v4("192.168..1").is_err());
    }

    #[test]
    fn parse_ip_v6_test() {
        assert_eq!(
            parse_ip_v6("2001:0db8:85a3:0000:0000:8a2e:0370:7334").unwrap(),
            (
                "",
                [0x2001, 0x0db8, 0x85a3, 0, 0, 0x8a2e, 0x370, 0x7334].into()
            )
        );
        assert_eq!(
            parse_ip_v6("2001:0db8:0000:0000:0000:0000:0000:0001").unwrap(),
            ("", [0x2001, 0x0db8, 0, 0, 0, 0, 0, 1].into())
        );

        assert_eq!(
            parse_ip_v6("2001:0db8:0000:0000:0000:ff00:0042:8329").unwrap(),
            ("", [0x2001, 0x0db8, 0, 0, 0, 0xff00, 0x42, 0x8329].into())
        );

        assert_eq!(
            parse_ip_v6("2001:db8:0:0:0:ff00:42:8329").unwrap(),
            ("", [0x2001, 0x0db8, 0, 0, 0, 0xff00, 0x42, 0x8329].into())
        );

        assert!(parse_ip_v6("2001:db8::::ff00:42:8329").is_err());
        assert_eq!(
            parse_ip_v6("::ffff:192.0.2.128").unwrap(),
            ("", [0, 0, 0, 0, 0, 0xffff, 0xc000, 0x280].into())
        );
        let test_cases = [
            (
                "2001:0db8:85a3:0000:0000:8a2e:0370:7334",
                vec![
                    0x2001, 0x0db8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334,
                ],
            ),
            (
                "2001:db8:85a3::8a2e:370:7334",
                vec![
                    0x2001, 0xdb8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334,
                ],
            ),
            (
                "2001:db8:85a3:0:0:8a2e:0370:7334",
                vec![
                    0x2001, 0xdb8, 0x85a3, 0x0000, 0x0000, 0x8a2e, 0x0370, 0x7334,
                ],
            ),
            (
                "2001:db8::370:7334",
                vec![0x2001, 0xdb8, 0x0, 0x0, 0x0, 0x0, 0x370, 0x7334],
            ),
            (
                "2001:0db8:0000:0000:0000:ff00:0042:8329",
                vec![0x2001, 0x0db8, 0x0, 0x0, 0x0, 0xff00, 0x42, 0x8329],
            ),
            (
                "fe80::1ff:fe23:4567:890a",
                vec![0xfe80, 0x0, 0x0, 0x0, 0x1ff, 0xfe23, 0x4567, 0x890a],
            ),
            (
                "0:0:0:0:0:0:0:0",
                vec![0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0],
            ),
            (
                "0:0:0:0:0:0:0:1",
                vec![0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x1],
            ),
            (
                "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff",
                vec![
                    0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff, 0xffff,
                ],
            ),
        ];

        for (addr, expected) in test_cases.into_iter() {
            let result = parse_ip_v6(addr).unwrap();
            assert_eq!(result, ("", expected),);
        }

        assert!(parse_ip_v6("192.168.1.1:0::ffff").is_err());
    }
}
