Information hiding
==================

First of all, we should not expect the participants to have deep
knowledge of network protocols.  After all, this is a PL centric
contest.  But network protocols are so much fun.

* DNS has all sorts of unused query types (or those with arbitrary data - from the domain (base32), over TXT, ...) - it is a distributed key/value store
* we can use Morse code encoding in the IP evil bit
* in URLs http://glench.com/hash/
* PGP web of trust - arbitrary signatures and comments
* Steganography in pictures / texts / arbitrary file formats
* in TLS certainly by using specific random/time (32 bytes per server hello!), reserved ciphersuites, session ids (another 32 bytes!)

