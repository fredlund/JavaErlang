#include "erl_nif.h"
#include <stdio.h>

static void our_destructor(ErlNifEnv *, void *);

static ErlNifResourceType *our_resource = NULL;

struct resource_term {
    ERL_NIF_TERM term;
    ErlNifPid pid;
    ErlNifEnv* env;
};

static ERL_NIF_TERM create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if (argc != 2 || !enif_is_pid(env,argv[1])) return enif_make_badarg(env);

    ErlNifEnv* our_env =
        enif_alloc_env();
    struct resource_term *resource_term =
        (struct resource_term *)
        enif_alloc_resource(our_resource, sizeof(struct resource_term));
    resource_term->term =
        enif_make_copy(our_env, argv[0]);
    resource_term->env =
        our_env;

    if (enif_get_local_pid(our_env, argv[1], &(resource_term->pid)) == 0) {
        enif_free_env(our_env);
        /* Leaks...but should never come here */
        return enif_make_badarg(env);
    }

    ERL_NIF_TERM term =
        enif_make_resource(env,resource_term);
    enif_release_resource(resource_term);
    return term;
}

static int on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    ErlNifResourceFlags tried;

    our_resource =
        enif_open_resource_type
        (env,
         NULL,
         "java_resource",
         our_destructor,
         ERL_NIF_RT_CREATE,
         &tried);

    return 0;
}

static void our_destructor(ErlNifEnv *env, void *destr_obj) {
    struct resource_term *resource_term =
        (struct resource_term *) destr_obj;
    enif_send
        (NULL,
         &(resource_term->pid),
         resource_term->env,
         resource_term->term);
    enif_free_env(resource_term->env);
}

static ErlNifFunc nif_funcs[] =
    {
        {"create", 2, create}
    };

ERL_NIF_INIT(java_resource,nif_funcs,&on_load,NULL,NULL,NULL)
