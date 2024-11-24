#![allow(unused)]

use std::{collections::VecDeque, ops::RangeBounds};

use chrono::ParseResult;
use nom::{
    bytes::complete::take_while_m_n,
    character::complete::one_of,
    combinator::{success, verify},
    error::{ParseError, VerboseError},
    multi::{many1, many_m_n},
};

use crate::prelude::*;

pub enum Segment {
    Hextet(u16),
    Compressed,
    IpV4(Vec<u8>),
}
fn parse_ip_v6(s: &str) -> ParserResult<Vec<u16>> {
    fn hex_to_u16(input: &str) -> Result<u16, std::num::ParseIntError> {
        u16::from_str_radix(input, 16)
    }
    fn recognize_hexadecimal(input: &str) -> ParserResult<&str> {
        recognize(take_while_m_n(1, 4, |c: char| c.is_ascii_hexdigit()))(input)
    }
    fn hextet(s: &str) -> ParserResult<u16> {
        map_res(recognize_hexadecimal, hex_to_u16)(s)
    };
    fn segment(s: &str) -> ParserResult<Segment> {
        alt((
            map(tag("::"), |_| Segment::Compressed),
            preceded(tag(":"), map(parse_ip_v4, Segment::IpV4)),
            preceded(opt(tag(":")), map(hextet, Segment::Hextet)),
        ))(s)
    }
    let mut ipv6: Vec<u16> = vec![];
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
            Segment::IpV4(l) => {
                ipv6.push((l[0] as u16) << 8 | l[1] as u16);
                ipv6.push((l[2] as u16) << 8 | l[3] as u16);
            }
        }
    }
    if let Some(idx) = compression_pos {
        let len = ipv6.len();
        while ipv6.len() < 8 {
            ipv6.insert(idx, 0x0);
        }
    }

    Ok((rest, ipv6))
}
fn parse_ip_v4(s: &str) -> ParserResult<Vec<u8>> {
    verify(
        separated_list1(
            tag("."),
            verify(
                map_parser(take_while1(|c: char| c.is_numeric()), all_consuming(U8)),
                |num: &u8| num <= &255,
            ),
        ),
        |list: &[u8]| list.len() == 4,
    )(s)
}

#[cfg(test)]
mod test {
    use crate::iri::{parse_ip_v4, parse_ip_v6};

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
    }
}
