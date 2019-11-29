#
import json
import sys

sys.path.append('lisp/')


def test_undefined_object01():
    from ein_remote_safe import __ein_maybe_undefined_object
    # Set up some user variables
    a = 1
    b = "A string"
    c = [1, 2, 3]
    obj_a = __ein_maybe_undefined_object('a', locals=locals())
    obj_b = __ein_maybe_undefined_object('b', locals=locals())
    obj_c = __ein_maybe_undefined_object('c', locals=locals())
    assert obj_a == a
    assert obj_b == b
    assert obj_c == c


def test_undefined_object02():
    from ein_remote_safe import __ein_maybe_undefined_object
    # Test on objects that don't exist
    obj_a = __ein_maybe_undefined_object('not_an_object', locals=locals())
    assert obj_a == None


def test_get_object_info01():
    from ein_remote_safe import __ein_object_info_for
    a = [1, 2, 3]
    oinfo_a = __ein_object_info_for(a)
    assert oinfo_a['type_name'] == "list"
    assert oinfo_a['string_form'] == "[1, 2, 3]"


def test_get_oinfo_from_string():
    from ein_remote_safe import __ein_maybe_undefined_object, __ein_object_info_for
    a = [1, 2,3 ]
    oinfo_a = __ein_object_info_for(__ein_maybe_undefined_object('a', locals=locals()))
    assert oinfo_a['type_name'] == "list"
    assert oinfo_a['string_form'] == "[1, 2, 3]"


def test_generate_oinfo_data01():
    from ein_remote_safe import __ein_generate_oinfo_data
    a = 1
    b = "A string"
    c = [1, 2, 3]
    oinfos = __ein_generate_oinfo_data(['a', 'b', 'c'], locals=locals())
    assert oinfos[0]['type_name'] == "int"
    assert oinfos[1]['type_name'] == "str"
    assert oinfos[2]['type_name'] == "list"


def test_generate_oinfo_package01():
    import heapq
    from ein_remote_safe import __ein_generate_oinfo_data
    oinfos = __ein_generate_oinfo_data(dir(heapq), locals=locals())
    assert len(oinfos) == 8 # Not everything in the package will generate object info.
    assert oinfos[0]['type_name'] == "dict"


def test_generate_oinfo_package02():
    import sys
    from ein_remote_safe import __ein_generate_oinfo_data
    oinfos = __ein_generate_oinfo_data(dir(sys), locals=locals())
    assert len(oinfos) == 7
    assert oinfos[0]['type_name'] == "str"
