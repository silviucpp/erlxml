
PROJECT_NIF_NAME=erlxml_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
	LDFLAGS +=  -Wl,-U,_enif_alloc \
				-Wl,-U,_enif_free \
				-Wl,-U,_enif_alloc_resource \
				-Wl,-U,_enif_open_resource_type \
				-Wl,-U,_enif_release_resource \
				-Wl,-U,_enif_priv_data \
				-Wl,-U,_enif_self \
				-Wl,-U,_enif_consume_timeslice \
				-Wl,-U,_enif_inspect_binary \
				-Wl,-U,_enif_inspect_iolist_as_binary \
				-Wl,-U,_enif_is_binary \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_is_list \
				-Wl,-U,_enif_get_int \
				-Wl,-U,_enif_get_list_cell \
				-Wl,-U,_enif_get_resource \
				-Wl,-U,_enif_get_tuple \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_badarg \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_double \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_resource \
				-Wl,-U,_enif_make_string_len \
				-Wl,-U,_enif_make_tuple \
				-Wl,-U,_enif_make_list \
				-Wl,-U,_enif_make_ulong \
				-Wl,-U,_enif_get_ulong \
				-Wl,-U,_enif_make_list_cell \
				-Wl,-U,_enif_make_reverse_list
endif

CXXFLAGS += -DNDEBUG -I pugixml \
			-g -Wextra -Werror -Wno-missing-field-initializers -fno-exceptions -fno-rtti -std=c++11

LDFLAGS +=  -lstdc++
