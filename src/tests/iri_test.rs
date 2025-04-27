use crate::iri::{
    Authority, Host, IHierPart, IPath, IRI, RelativePart, RelativeRef,
    ip::{parse_ip_v4, parse_ip_v6},
};

#[test]
fn test_hex_st_to_char() {
    assert_eq!(':', u8::from_str_radix("3A", 16).unwrap() as char);
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

#[test]

fn test_iris() {
    let iri = IRI::try_from("http://example.com/").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "http",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("example.com",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/",],),
            },
            query: "",
            fragment: "",
        }
    );

    let iri = IRI::try_from("https://example.com/page").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "https",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("example.com",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/page",],),
            },
            query: "",
            fragment: "",
        }
    );

    let iri = IRI::try_from("ftp://ftp.example.org/file.txt").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "ftp",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("ftp.example.org",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/file.txt",],),
            },
            query: "",
            fragment: "",
        }
    );

    let iri = IRI::try_from("http://example.com/a/b%20c").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "http",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("example.com",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/a", "/b%20c"],),
            },
            query: "",
            fragment: "",
        }
    );

    let iri = IRI::try_from("http://example.com/a/こんにちは").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "http",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("example.com",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/a", "/こんにちは"],),
            },
            query: "",
            fragment: "",
        }
    );

    let iri = IRI::try_from("mailto:user@example.com").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "mailto",
            hier_part: IHierPart::Rootless(IPath::Rootless {
                snz: "user@example.com",
                segments: vec![]
            }),
            query: "",
            fragment: "",
        }
    );

    let iri = IRI::try_from("http://example.com/?q=foo%3Dbar").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "http",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("example.com",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/"],),
            },
            query: "q=foo%3Dbar",
            fragment: "",
        }
    );
    let iri = IRI::try_from("/a/b/c").unwrap();
    assert_eq!(
        iri,
        IRI::Reference(RelativeRef {
            relative_part: RelativePart::Absolute(IPath::Absolute {
                snz: "",
                segments: vec!["/a", "/b", "/c",],
            },),
            query: "",
            fragment: "",
        },)
    );

    let iri = IRI::try_from("./c").unwrap();
    assert_eq!(
        iri,
        IRI::Reference(RelativeRef {
            relative_part: RelativePart::NoScheme(IPath::NoScheme {
                snz_nc: ".",
                segments: vec!["/c",],
            },),
            query: "",
            fragment: "",
        },)
    );
    let iri = IRI::try_from("../b/c").unwrap();
    assert_eq!(
        iri,
        IRI::Reference(RelativeRef {
            relative_part: RelativePart::NoScheme(IPath::NoScheme {
                snz_nc: "..",
                segments: vec!["/b", "/c",],
            },),
            query: "",
            fragment: "",
        },)
    );
    let iri = IRI::try_from("http://xn--fsq.com").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "http",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("xn--fsq.com",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec![],),
            },
            query: "",
            fragment: "",
        }
    );
    let iri = IRI::try_from("http://[2001:db8::1]/path").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "http",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::IPV6(vec![0x2001, 0xdb8, 0, 0, 0, 0, 0, 1]),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/path"],),
            },
            query: "",
            fragment: "",
        }
    );
    let iri = IRI::try_from("ftp://example.com/path?query=1&param=2#fragment").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "ftp",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: None,
                    host: Host::RegName(Some("example.com",),),
                    port: None,
                },
                ipath: IPath::AbEmpty(vec!["/path"],),
            },
            query: "query=1&param=2",
            fragment: "fragment",
        }
    );
    let iri = IRI::try_from("http://user:pass@example.com:8080/path?q#frag").unwrap();
    assert_eq!(
        iri,
        IRI::IRI {
            scheme: "http",
            hier_part: IHierPart::AbEmpty {
                authority: Authority {
                    user_info: Some("user:pass"),
                    host: Host::RegName(Some("example.com")),
                    port: Some("8080"),
                },
                ipath: IPath::AbEmpty(vec!["/path"],),
            },
            query: "q",
            fragment: "frag",
        }
    );
    assert!(IRI::try_from("://example.com").is_err());
    assert!(IRI::try_from("`http://example.com/").is_err());
    assert!(IRI::try_from("http://example.com/\n/path").is_err());
    let iri = IRI::try_from("path/").unwrap();
    assert_eq!(
        iri,
        IRI::Reference(RelativeRef {
            relative_part: RelativePart::NoScheme(IPath::NoScheme {
                snz_nc: "path",
                segments: vec!["/",],
            },),
            query: "",
            fragment: "",
        },)
    );
    assert_eq!(
        IRI::try_from("about").unwrap(),
        IRI::Reference(RelativeRef {
            relative_part: RelativePart::NoScheme(IPath::NoScheme {
                snz_nc: "about",
                segments: vec![],
            },),
            query: "",
            fragment: "",
        },)
    );
    assert_eq!(
        IRI::try_from("").unwrap(),
        IRI::Reference(RelativeRef {
            relative_part: RelativePart::Empty(IPath::Empty),
            query: "",
            fragment: "",
        },)
    );
}
