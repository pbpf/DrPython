//PyLongObject 
///This subtype of PyObject represents a Python integer object.

//PyTypeObject PyLong_Type 
//This instance of PyTypeObject represents the Python integer type. This is the same object as int in the Python layer.
typedef scheme_integer_type  PyLong_Type;
//int PyLong_Check(PyObject *p) 
#define  PyLong_Check(x) SCHEME_INTP(x)
//Return true if its argument is a PyLongObject or a subtype of PyLongObject.
#define  PyLong_CheckExact(x) SCHEME_INTP(x)
//int PyLong_CheckExact(PyObject *p) 
//Return true if its argument is a PyLongObject, but not a subtype of PyLongObject.
#define PyLong_FromLong(x) scheme_make_integer(x)
//PyObject* PyLong_FromLong(long v) 
//Scheme_Object* scheme_make_integer(intptr_t i)
//Return value: New reference.
//Return a new PyLongObject object from v, or NULL on failure.

//The current implementation keeps an array of integer objects for all integers between -5 and 256, when you create an int in that range you actually just get back a reference to the existing object. So it should be possible to change the value of 1. I suspect the behaviour of Python in this case is undefined. :-)
#define PyLong_FromUnsignedLong(v) scheme_make_integer_value_from_unsigned(v)

//PyObject* PyLong_FromUnsignedLong(unsigned long v) 
//Return value: New reference.
//Return a new PyLongObject object from a C unsigned long, or NULL on failure.

//Py_ssize_t!!!!!!!!!!!!!!!
#define  PyLong_FromSsize_t(v) scheme_make_integer(v)
//Return a new PyLongObject object from a C Py_ssize_t, or NULL on failure.

#define PyLong_FromSize_t(v) scheme_make_integer(v)
//Return a new PyLongObject object from a C size_t, or NULL on failure.
#define PyLong_FromLongLong(a) scheme_make_integer_value_from_long_long(a)
//PyObject* PyLong_FromLongLong(PY_LONG_LONG v) 
//Return value: New reference.
//Return a new PyLongObject object from a C long long, or NULL on failure.
#define PyLong_FromUnsignedLongLong(a) scheme_make_integer_value_from_unsigned_long_long(a)

PyObject* PyLong_FromUnsignedLongLong(unsigned PY_LONG_LONG v) 
//Return value: New reference.
//Return a new PyLongObject object from a C unsigned long long, or NULL on failure.

#define PyLong_FromDouble(v) scheme_make_integer(int (v))
//Return value: New reference.
//Return a new PyLongObject object from the integer part of v, or NULL on failure.

PyObject* PyLong_FromString(const char *str, char **pend, int base); 
//Return value: New reference.
//Return a new PyLongObject based on the string value in str, which is interpreted according to the radix in base. If pend is non-NULL, *pend will point to the first character in str which follows the representation of the number. If base is 0, the radix will be determined based on the leading characters of str: if str starts with '0x' or '0X', radix 16 will be used; if str starts with '0o' or '0O', radix 8 will be used; if str starts with '0b' or '0B', radix 2 will be used; otherwise radix 10 will be used. If base is not 0, it must be between 2 and 36, inclusive. Leading spaces are ignored. If there are no digits, ValueError will be raised.

PyObject* PyLong_FromUnicode(Py_UNICODE *u, Py_ssize_t length, int base); 
//Return value: New reference.
//Convert a sequence of Unicode digits to a Python integer value. The Unicode string is first encoded to a byte string using PyUnicode_EncodeDecimal() and then converted using PyLong_FromString().

//Deprecated since version 3.3, will be removed in version 4.0: Part of the old-style Py_UNICODE API; please migrate to using PyLong_FromUnicodeObject().

PyObject* PyLong_FromUnicodeObject(PyObject *u, int base) 
//Convert a sequence of Unicode digits in the string u to a Python integer value. The Unicode string is first encoded to a byte string using PyUnicode_EncodeDecimal() and then converted using PyLong_FromString().

//New in version 3.3.

#define PyLong_FromVoidPtr(p) scheme_make_integer(int(p))
//PyObject* PyLong_FromVoidPtr(void *p); 
//Return value: New reference.
//Create a Python integer from the pointer p. The pointer value can be retrieved from the resulting value using PyLong_AsVoidPtr().

long PyLong_AsLong(PyObject *obj)
{
	long x;
    if(scheme_get_int_val(pylong,&x))
		return x;
	else 
		return NULL;////
};
//Return a C long representation of obj. If obj is not an instance of PyLongObject, first call its __int__() method (if present) to convert it to a PyLongObject.

//Raise OverflowError if the value of obj is out of range for a long.

//long PyLong_AsLongAndOverflow(PyObject *obj, int *overflow); 
//Return a C long representation of obj. If obj is not an instance of PyLongObject, first call its __int__() method (if present) to convert it to a PyLongObject.

//If the value of obj is greater than LONG_MAX or less than LONG_MIN, set *overflow to 1 or -1, respectively, and return -1; otherwise, set *overflow to 0. If any other exception occurs set *overflow to 0 and return -1 as usual.

PY_LONG_LONG PyLong_AsLongLong(PyObject *obj); 
//Return a C long long representation of obj. If obj is not an instance of PyLongObject, first call its __int__() method (if present) to convert it to a PyLongObject.

//Raise OverflowError if the value of obj is out of range for a long.

PY_LONG_LONG PyLong_AsLongLongAndOverflow(PyObject *obj, int *overflow); 
//Return a C long long representation of obj. If obj is not an instance of PyLongObject, first call its __int__() method (if present) to convert it to a PyLongObject.

//If the value of obj is greater than PY_LLONG_MAX or less than PY_LLONG_MIN, set *overflow to 1 or -1, respectively, and return -1; otherwise, set *overflow to 0. If any other exception occurs set *overflow to 0 and return -1 as usual.

//New in version 3.2.

Py_ssize_t PyLong_AsSsize_t(PyObject *pylong);
//Return a C Py_ssize_t representation of pylong. pylong must be an instance of PyLongObject.

//Raise OverflowError if the value of pylong is out of range for a Py_ssize_t.

unsigned long PyLong_AsUnsignedLong(PyObject *pylong)
{
	unsigned long x;
    if(scheme_get_unsigned_int_val(pylong,&x))
		return x;
	else 
		return NULL;////
};
//Return a C unsigned long representation of pylong. pylong must be an instance of PyLongObject.

//Raise OverflowError if the value of pylong is out of range for a unsigned long.

size_t PyLong_AsSize_t(PyObject *pylong); 
//Return a C size_t representation of pylong. pylong must be an instance of PyLongObject.

//Raise OverflowError if the value of pylong is out of range for a size_t.

unsigned PY_LONG_LONG PyLong_AsUnsignedLongLong(PyObject *pylong); 
//Return a C unsigned PY_LONG_LONG representation of pylong. pylong must be an instance of PyLongObject.

//Raise OverflowError if the value of pylong is out of range for an unsigned PY_LONG_LONG.

//Changed in version 3.1: A negative pylong now raises OverflowError, not TypeError.

unsigned long PyLong_AsUnsignedLongMask(PyObject *obj); 
//Return a C unsigned long representation of obj. If obj is not an instance of PyLongObject, first call its __int__() method (if present) to convert it to a PyLongObject.

//If the value of obj is out of range for an unsigned long, return the reduction of that value modulo ULONG_MAX + 1.

unsigned PY_LONG_LONG PyLong_AsUnsignedLongLongMask(PyObject *obj); 
//Return a C unsigned long long representation of obj. If obj is not an instance of PyLongObject, first call its __int__() method (if present) to convert it to a PyLongObject.

//If the value of obj is out of range for an unsigned long long, return the reduction of that value modulo PY_ULLONG_MAX + 1.

double PyLong_AsDouble(PyObject *pylong); 
//Return a C double representation of pylong. pylong must be an instance of PyLongObject.

//Raise OverflowError if the value of pylong is out of range for a double.

void* PyLong_AsVoidPtr(PyObject *pylong); 
//Convert a Python integer pylong to a C void pointer. If pylong cannot be converted, an OverflowError will be raised. This is only assured to produce a usable void pointer for values created with PyLong_FromVoidPtr().
