mod dns;
mod ethernet;
mod http;

use clap::{App, Arg};
use smoltcp::phy::TapInterface;
use url::Url;

fn main() {
    let app = App::new("mget")
        .about("GET a webpage, manually")
        .arg(Arg::with_name("url").required(true))
        .arg(Arg::with_name("tap-device").required(true))
        .arg(
            Arg::with_name("dns-server")
                .short("s")
                .default_value("8.8.8.8"),
        )
        .get_matches();

    let url_text = app.value_of("url").unwrap();
    let dns_server_text = app.value_of("dns-server").unwrap();
    let tap_text = app.value_of("tap-device").unwrap();

    let url = Url::parse(url_text).expect("Unable to parse <url> as a URL");
    if url.scheme() != "http" {
        eprintln!("Only HTTP protocol supported");
        return;
    }
    let domain_name = url.host_str().expect("Domain name required");

    let _dns_server: std::net::Ipv4Addr = dns_server_text
        .parse()
        .expect("Unable to parse <dns-server> as an IPv4 address");
    let tap =
        TapInterface::new(&tap_text).expect("Unable to use <tap-device> as a network interface");
    let addr = dns::resolve(dns_server_text, domain_name).unwrap().unwrap();
    let mac: smoltcp::wire::EthernetAddress = ethernet::MacAddress::new().into();

    http::get(tap, mac, addr, url).unwrap();
}
