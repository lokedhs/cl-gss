cl-gss - Implementation of GSS-API bindings for Common Lisp

Author contact information
==========================

Elias Martenson
Email: lokedhs@gmail.com

Examples
========

On the initiating side, the function INIT-SEC must be called to
initialise the handshake. This function takes a single required
parameter, the service name of service to which you intend to connect.

```lisp
  (cl-gss:init-sec "host@domain" :flags '(:mutual))
```

In this case, we're only passing a single flag, :MUTUAL. This flag
indicates that not only do I want to verify my identify with the
remote service. It should identify itself with me.

The function returns several values:
