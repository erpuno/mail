Feed Server
===========

Overview
--------

Feed Server is node of user workers pool (shards). It handles all MQ requests for
write operations for user's data and other APIs. Also it stores a cache for user data.
Feed Server manages spawning those workers on startup and supervising them.
You could create your own workers based on consumer behavior provided by Feed Server.
Feed Server is a product and can be used 

Dependencies
------------

* white_rabbit (RabbitMQ client library)
* store_framework (KV storages abstraction layer)

Credits
-------

Maxim Sokhatsky
Andrii Zadorozhnii

OM A HUM
