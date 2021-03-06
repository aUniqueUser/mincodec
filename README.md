# mincodec

[Documentation](https://noocene.github.io/mincodec)

Highly spatially efficient serialization format supporting `#[no_std]` and making zero allocations.

#### comparison
All sizes listed in bits
|Type | Value | mincodec | CBOR | bincode | MessagePack | Postcard|
|--- | --- | --- | --- | --- | --- | ---|
|`Option<bool>`|`Some(true)`|2|8|16|8|16|
|`Option<bool>`|`Some(false)`|2|8|16|8|16|
|`Option<bool>`|`None`|1|8|8|8|8|
|`String`|`"hello"`|48|48|104|48|48|
|`Vec<bool>`|`[true, false, true]`|11|32|88|32|32|
|`(bool, u8)`|`(false, 3)`|9|24|16|24|16|
|`CustomOption<bool>`|`Some(true)`|2|56|40|24|16|
|`()`|`()`|0|8|0|8|0|
|`[String; 0]`|`[]`|0|8|0|8|0|
|`[bool; 2]`|`[true, false]`|2|24|16|24|16|
|`Bit`|`High`|1|40|32|24|8|
|`OneVariant`|`Variant`|0|64|32|24|8|

```Rust
pub enum CustomOption<T> {
    Some(T),
    None,
}

pub enum Bit {
    High,
    Low,
}

pub enum OneVariant {
    Variant,
}
```

#### features
- True frameless design (no headers/prefixing/etc. in multi-item streams)
- Perfect incremental deserialization (intermediate buffers are filled per-bit as data becomes available), no cycles are wasted re-deserializing data
- `#![no_std]` support using [core-futures-io](https://github.com/noocene/core-futures-io)
- Compatibility with both `tokio` and `async_std` using compatibility adapters from core-futures-io
- Bit-level serialization using [bitbuf](https://github.com/noocene/bitbuf) facilitates the extreme spatial efficiency displayed above
- Efficient size prefixing for DSTs using [bitbuf-vlq](https://github.com/noocene/bitbuf-vlq)
- Asynchronicity permits deserialization of data as it becomes available over transport, spreading the time cost of deserialization or serialization over the transport's bandwidth domain
- Asynchronicity permits dependency of serialized/deserialized types on futures, the dual of transport-side asynchronicity
- `serde`-style derive macro for straightforward implementation with support for generic parametrization

#### async_std TcpStream example
```Rust
use async_std::net::{TcpStream, TcpListener};
use core_futures_io::FuturesCompat;
use futures::{executor::block_on, StreamExt};
use mincodec::{AsyncWriter, MinCodec, AsyncReader};
use std::thread::spawn;

#[derive(MinCodec, Debug)]
enum Permission {
    Read,
    Write,
    Invite { remaining: u32 },
}

#[derive(MinCodec, Debug)]
struct Capability {
    permission: Permission,
    domain: String,
}

#[derive(MinCodec, Debug)]
struct User {
    name: String,
    capabilities: Vec<Capability>,
}

#[derive(MinCodec, Debug)]
enum Action<T> {
    Create(T),
    Remove { index: u32 },
}

#[derive(MinCodec, Debug)]
struct Post {
    content: String,
    date: u64,
}

#[derive(MinCodec, Debug)]
struct Data {
    user: User,
    action: Action<Post>,
}

fn main() {
    let thread = spawn(|| block_on(async {
        let listener = TcpListener::bind("127.0.0.1:8080").await.expect("could not bind listener");
        let mut incoming = listener.incoming();

        if let Some(stream) = incoming.next().await {
            let stream = stream.expect("listener-side connection error");
            println!("received: {:?}", AsyncReader::<_, Data>::new(FuturesCompat::new(stream)).await.expect("error reading data and deserializing"));
        }
    }));
    block_on(async {
        let stream = TcpStream::connect("127.0.0.1:8080")
            .await
            .expect("could not connect");
            
        AsyncWriter::new(
            FuturesCompat::new(stream),
            Data {
                action: Action::Create(Post {
                    content: "Hello there!".to_owned(),
                    date: 1230192809,
                }),
                user: User {
                    name: "J. Doe".to_owned(),
                    capabilities: vec![
                        Capability {
                            permission: Permission::Write,
                            domain: "posts".to_owned(),
                        },
                        Capability {
                            permission: Permission::Read,
                            domain: "posts".to_owned(),
                        },
                    ],
                },
            },
        )
        .await
        .expect("could not serialize and transmit");
    });
    thread.join().unwrap();
}
```