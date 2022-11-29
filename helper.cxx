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
    char subdir[6];
    subdir[0]=cid[len-2];
    subdir[1]=cid[len-1];
    subdir[2]='/';
    subdir[3]=cid[len-4];
    subdir[4]=cid[len-3];
    subdir[5]='\0';
    CAMLreturn(caml_alloc_initialized_string(5, subdir));
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
extern value cpp_encrypt_aes256(value viv, value vk, value varr);
} // extern C

extern "C" {
extern value cpp_decrypt_aes256(value viv, value vk, value varr);
} // extern C
