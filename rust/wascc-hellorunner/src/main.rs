use std::collections::HashMap;
use wascc_host::{Actor, NativeCapability, WasccHost};

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let _ = env_logger::try_init();
    let host = WasccHost::new();

    host.add_actor(Actor::from_file("../hellohttp/hello_signed.wasm")?)?;
    host.add_native_capability(NativeCapability::from_file("/Users/jiayihu/Desktop/Repo/wascc/http-server-provider/target/debug/libwascc_httpsrv.dylib", None)?)?;

    host.bind_actor(
        "MBPHVHCZX37IRQD5BG5AZ3SRVQAPCENYZMEKIKN6WRMX2I557FJT4ENJ",
        "wascc:http_server",
        None,
        generate_port_config(8081),
    )?;

    std::thread::park();

    Ok(())
}

fn generate_port_config(port: u16) -> HashMap<String, String> {
    let mut hm = HashMap::new();
    hm.insert("PORT".to_string(), port.to_string());

    hm
}
