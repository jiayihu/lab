use std::error::Error;
use std::net::{SocketAddr, UdpSocket};
use std::time::Duration;

use std::fmt;
use trust_dns::op::{Message, MessageType, OpCode, Query};
use trust_dns::proto::error::ProtoError;
use trust_dns::rr::domain::Name;
use trust_dns::rr::record_type::RecordType;
use trust_dns::serialize::binary::*;

fn message_id() -> u16 {
    let candidate = rand::random();
    if candidate == 0 {
        return message_id();
    }

    candidate
}

#[derive(Debug)]
pub enum DnsError {
    ParseDomainName(ProtoError),
    ParseDnsServerAddress(std::net::AddrParseError),
    Encoding(ProtoError),
    Decoding(ProtoError),
    Network(std::io::Error),
    Sending(std::io::Error),
    Receiving(std::io::Error),
}

impl std::fmt::Display for DnsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for DnsError {
    // Noop
}

pub fn resolve(
    dns_server_address: &str,
    domain_name: &str,
) -> Result<Option<std::net::IpAddr>, Box<dyn Error>> {
    let mut request_as_bytes: Vec<u8> = Vec::with_capacity(512);
    let mut response_as_bytes: [u8; 512] = [0; 512];

    let domain_name = Name::from_ascii(domain_name).map_err(DnsError::ParseDomainName)?;

    let mut msg = Message::new();
    msg.set_id(message_id())
        .set_message_type(MessageType::Query)
        .add_query(Query::query(domain_name, RecordType::A))
        .set_op_code(OpCode::Query)
        .set_recursion_desired(true);

    let mut encoder = BinEncoder::new(&mut request_as_bytes);
    msg.emit(&mut encoder).map_err(DnsError::Encoding)?;

    let localhost = UdpSocket::bind("0.0.0.0:0").map_err(DnsError::Network)?;
    let timeout = Duration::from_secs(3);
    localhost
        .set_read_timeout(Some(timeout))
        .map_err(DnsError::Network)?;
    localhost
        .set_nonblocking(false)
        .map_err(DnsError::Network)?;

    let dns_server: SocketAddr = format!("{}:53", dns_server_address)
        .parse()
        .map_err(DnsError::ParseDnsServerAddress)?;
    let _amt = localhost
        .send_to(&request_as_bytes, dns_server)
        .map_err(DnsError::Sending);

    loop {
        let (_amt, remote_port) = localhost
            .recv_from(&mut response_as_bytes)
            .map_err(DnsError::Receiving)?;

        if remote_port == dns_server {
            break;
        }
    }

    let dns_message = Message::from_vec(&response_as_bytes).map_err(DnsError::Decoding)?;

    for answer in dns_message.answers() {
        if answer.record_type() == RecordType::A {
            let resource = answer.rdata();
            let ip = resource.to_ip_addr().expect("Invalid IP address received");

            return Ok(Some(ip));
        }
    }

    Ok(None)
}
