use libactionkv::{ActionKV, ByteStr, ByteString};
use std::collections::HashMap;

#[cfg(target_os = "windows")]
const USAGE: &'static str = "
Usage:
    akv_mem.exe FILE get KEY
    akv_mem.exe FILE delete KEY
    akv_mem.exe FILE insert KEY VALUE
    akv_mem.exe FILE update KEY VALUE
";

#[cfg(not(target_os = "windows"))]
const USAGE: &'static str = "
Usage:
    akv_mem FILE get KEY
    akv_mem FILE delete KEY
    akv_mem FILE insert KEY VALUE
    akv_mem FILE update KEY VALUE
";

fn store_index_on_disk(store: &mut ActionKV, index_key: &ByteStr) {
    store.index.remove(index_key);

    let index_as_bytes = bincode::serialize(&store.index).unwrap();
    store.index = std::collections::HashMap::new();
    store.insert(index_key, &index_as_bytes).unwrap();
}

fn main() {
    const INDEX_KEY: &ByteStr = b"+index";

    let args: Vec<String> = std::env::args().collect();
    let fname = args.get(1).expect(&USAGE);
    let action = args.get(2).expect(&USAGE).as_ref(); // As &str
    let key = args.get(3).expect(&USAGE).as_ref();
    let maybe_value = args.get(4);

    let path = std::path::Path::new(&fname);
    let mut store = ActionKV::open(path).expect("Unable to open file");
    store.load().expect("Unable to load data");
    store_index_on_disk(&mut a, INDEX_KEY);

    match action {
        "get" => {
            let index_as_bytes = store.get(&INDEX_KEY).unwrap().unwrap();
            let index: HashMap<ByteString, u64> = bincode::deserialize(&index_as_bytes).unwrap();

            match index.get(key) {
                None => eprintln!("{:?} not found", key),
                Some(value) => println!("{:?}", value),
            }
        }
        "delete" => store.delete(key).unwrap(),
        "insert" => {
            let value = maybe_value.expect(&USAGE).as_ref();
            store.insert(key, value).unwrap()
        }
        "update" => {
            let value = maybe_value.expect(&USAGE).as_ref();
            store.update(key, value).unwrap()
        }
        _ => eprintln!("{}", &USAGE),
    }
}
