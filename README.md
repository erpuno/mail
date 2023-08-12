SYNRC ✉️ MAIL
=============

Open source lightweight Messaging Handling Service (MHS X.419)
and Interpersonal Messaging System (IPMS X.420).
MHS was a very open system, it was popular in the
early 1990s as a glue between proprietary
email systems, competing standards-based SMTP and X.400.
However, by 1996 it was clear that SMTP over the Internet
would take over this role. However MHS/IPMS is a faster
and has more telecomunication flavour than SMTP track of standards.
Now MHS is used in avionics and in Military Message Handling
System (MMHS, RFC 6477).

Features
--------

* Databases: MNESIA, ROCKSDB
* Pub/Sub GPROC, SYN
* Formatters BASE64, BERT, BER/DER/PER [ASN.1]

Intro
-----

MAIL is an QoS=1 example of messaging system built on top of:

* SYN for publish subscribe message queue;
* N2O for protocols;
* KVS for data storage;
* ASN1 for encoding.

It also contains simple textual WebSocket protocol for debugging purposes.
You can freely use this example with your favourite formatter for user terminal protocol.

```shell
```

Credits
-------

* Namdak Tonpa

OM A HUM
