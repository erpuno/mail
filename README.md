Feeds
===========

Feed Server
--------

Feed Server is node of user workers pool (shards). It handles all MQ requests for
write operations for user's data and other APIs. Also it stores a cache for user data.
Feed Server manages spawning those workers on startup and supervising them.
You could create your own workers based on consumer behavior provided by Feed Server.

Feed Server is a product that can be used for:

* Handling partitioned distributed cache
* Distribution worker proccesses connected to MQ bus
* Providing sequential consistency for DHT
* Notifications handlers: mailers, background workers, etc.

Feed UI
--------

The set of modules to render the chain of objects (feed) and corresponding input control.

Dependencies
------------

* mqs (RabbitMQ client library)
* kvs (KV storages abstraction layer)

Credits
-------

* Maxim Sokhatsky
* Andrii Zadorozhnii

OM A HUM
