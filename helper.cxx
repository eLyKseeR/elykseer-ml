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

#include <cstring>

extern "C" {
value cpp_buffer_id(value vbuf)
{
    return vbuf;
}
} // extern C

extern "C" {
value cpp_mk_cid_subdir(value vcid)
{
    CAMLparam1(vcid);
    const char *cid = String_val(vcid);
    int len = caml_string_length(vcid);
    char subdir[3];
    subdir[0]=cid[len-2];
    subdir[1]=cid[len-1];
    subdir[2]='\0';
    CAMLreturn(caml_alloc_initialized_string(2, subdir));
}
} // extern C

struct _cpp_cstdio_buffer {
    char *_buf {nullptr};
    long _len {0};
};

#define CPP_CSTDIO_BUFFER(v) (*((_cpp_cstdio_buffer**) Data_custom_val(v)))

extern void del_cpp_cstdio_buffer (value v);

static struct custom_operations cpp_cstdio_buffer_ops = {
    (char *)"mlcpp_cstdio_buffer",
    del_cpp_cstdio_buffer,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

extern "C" {
value cpp_encrypt_buffer(value vbuf, value vpw)
{
    CAMLparam2(vbuf, vpw);
    CAMLlocal1(res);
    res = caml_alloc_custom(&cpp_cstdio_buffer_ops,
                            sizeof(_cpp_cstdio_buffer*), 1, 10000);
    const struct _cpp_cstdio_buffer *s = CPP_CSTDIO_BUFFER(vbuf);
    struct _cpp_cstdio_buffer *t = new _cpp_cstdio_buffer;
    t->_buf = (char*)calloc(s->_len, 1);
    t->_len = s->_len;

    // TODO: encrypt buffer
    std::memcpy(t->_buf, s->_buf, t->_len);

    CPP_CSTDIO_BUFFER(res) = t;

    return res;
}
} // extern C

extern "C" {
value cpp_decrypt_buffer(value vbuf, value vpw)
{
    CAMLparam2(vbuf, vpw);
    CAMLlocal1(res);
    res = caml_alloc_custom(&cpp_cstdio_buffer_ops,
                            sizeof(_cpp_cstdio_buffer*), 1, 10000);
    const struct _cpp_cstdio_buffer *s = CPP_CSTDIO_BUFFER(vbuf);
    struct _cpp_cstdio_buffer *t = new _cpp_cstdio_buffer;
    t->_buf = (char*)calloc(s->_len, 1);
    t->_len = s->_len;

    // TODO: decrypt buffer
    std::memcpy(t->_buf, s->_buf, t->_len);

    CPP_CSTDIO_BUFFER(res) = t;

    return res;
}
} // extern C
