use serde_json::json;
use wascc_actor::prelude::*;

actor_handlers! {
    codec::http::OP_HANDLE_REQUEST => hello_world,
    codec::core::OP_HEALTH_REQUEST => health
}

fn hello_world(_payload: codec::http::Request) -> HandlerResult<codec::http::Response> {
    let result = json!({ "hello": "world", "data": 42 });

    Ok(codec::http::Response::json(result, 200, "OK"))
}

fn health(_req: codec::core::HealthRequest) -> HandlerResult<()> {
    Ok(())
}
