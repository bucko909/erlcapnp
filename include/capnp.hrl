-record(capnp_context, {by_id, name_to_id}).
% Be sure offset is first in #field_info{} to get a nice ordering.
-record(field_info, {offset, type, name, default, discriminant, override}).
-record(native_type, {name, type, width, extra, binary_options, list_tag}).
-record(ptr_type, {type, extra}).
-record(group_type, {type_id}).
