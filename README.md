MAIL: Message Delivery System
=============================

TL;DR UNIX mail done right.

Features
--------

* Usage Example of N2O and KVS
* RocksDB support out of the box
* 150 LOC

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

Then run `wscat --no-check -c https://localhost:8042/ws`

Credits
-------

* Maxim Sokhatsky [5HT](https://github.com/5HT)
* Vlad Ki [proger](https://github.com/proger)

OM A HUM
