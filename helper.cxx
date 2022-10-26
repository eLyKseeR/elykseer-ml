// OCaml includes
extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
// #include <caml/callback.h>
#include <caml/fail.h>

#include <sys/errno.h>
} //extern C


extern "C" {
value cpp_buffer_id(value vbuf)
{
    return vbuf;
}
} // extern C

struct _cpp_cstdio_buffer {
    char *_buf {nullptr};
    long _len {0};
};

#define CPP_CSTDIO_BUFFER(v) (*((_cpp_cstdio_buffer**) Data_custom_val(v)))

extern "C" {
value cpp_encrypt_buffer(value vbuf, value vpw)
{
    CAMLparam2(vbuf, vpw);
    CAMLlocal1(res);
    struct _cpp_cstdio_buffer *s = CPP_CSTDIO_BUFFER(vbuf);

    return res;
}
} // extern C

extern "C" {
value cpp_decrypt_buffer(value vbuf, value vpw)
{
    return vbuf;
}
} // extern C
