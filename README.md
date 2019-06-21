CHAT: Instant Messaging
=======================

TL;DR CRM core example for Elixir.

Features
--------

* Usage Example of N2O and KVX
* RocksDB support out of the box

Prerequisites
-------------

* cmake (rocksdb)

Run
---

Before running, [fullchain.pem](./priv/ssl/fullchain.pem) certificate has to be added into a system.

```
$ mix deps.get
$ mix compile
$ iex -S mix
```

Then open `https://localhost:8042/app/index.html`

Credits
-------

* Maxim Sokhatsky [5HT](https://github.com/5HT)
* Vlad Ki [proger](https://github.com/proger)

OM A HUM
