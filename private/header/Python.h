#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include <assert.h> 
#include <stdlib.h>

//new Python.h that use racket core runtime

#include "escheme.h"

typedef Scheme_Object PyObject;

typedef PyObject* PyCFunction;

struct PyMethodDef
{
  char         *ml_name��    
  PyCFunction  ml_meth��    
  int          ml_flags��   
  char         ml_doc��      
};


PyObject* Py_InitModule(char * name, PyMethodDef *);
PyObject* Py_BuildValue(const char *format, ...);
PyObject* Py_VaBuildValue(const char *format, va_list vargs); 
int PyArg_ParseTuple(PyObject arg*, const char *format);
int PyArg_VaParse(PyObject *args, const char *format, va_list vargs); 
int PyArg_ParseTupleAndKeywords(PyObject *args, PyObject *kw, const char *format, char *keywords[], ...);
int PyArg_VaParseTupleAndKeywords(PyObject *args, PyObject *kw, const char *format, char *keywords[], va_list vargs);
int PyArg_ValidateKeywordArguments(PyObject *);
int PyArg_Parse(PyObject *args, const char *format, ...);
int PyArg_UnpackTuple(PyObject *args, const char *name, Py_ssize_t min, Py_ssize_t max, ...);
/* format=
 s (str or None) [char *] 
Convert a null-terminated C string to a Python str object using 'utf-8' encoding. If the C string pointer is NULL, None is used. 
s# (str or None) [char *, int] 
Convert a C string and its length to a Python str object using 'utf-8' encoding. If the C string pointer is NULL, the length is ignored and None is returned. 
y (bytes) [char *] 
This converts a C string to a Python bytes object. If the C string pointer is NULL, None is returned. 
y# (bytes) [char *, int] 
This converts a C string and its lengths to a Python object. If the C string pointer is NULL, None is returned. 
z (str or None) [char *] 
Same as s. 
z# (str or None) [char *, int] 
Same as s#. 
u (str) [Py_UNICODE *] 
Convert a null-terminated buffer of Unicode (UCS-2 or UCS-4) data to a Python Unicode object. If the Unicode buffer pointer is NULL, None is returned. 
u# (str) [Py_UNICODE *, int] 
Convert a Unicode (UCS-2 or UCS-4) data buffer and its length to a Python Unicode object. If the Unicode buffer pointer is NULL, the length is ignored and None is returned. 
U (str or None) [char *] 
Same as s. 
U# (str or None) [char *, int] 
Same as s#. 
i (int) [int] 
Convert a plain C int to a Python integer object. 
b (int) [char] 
Convert a plain C char to a Python integer object. 
h (int) [short int] 
Convert a plain C short int to a Python integer object. 
l (int) [long int] 
Convert a C long int to a Python integer object. 
B (int) [unsigned char] 
Convert a C unsigned char to a Python integer object. 
H (int) [unsigned short int] 
Convert a C unsigned short int to a Python integer object. 
I (int) [unsigned int] 
Convert a C unsigned int to a Python integer object. 
k (int) [unsigned long] 
Convert a C unsigned long to a Python integer object. 
L (int) [PY_LONG_LONG] 
Convert a C long long to a Python integer object. Only available on platforms that support long long (or _int64 on Windows). 
K (int) [unsigned PY_LONG_LONG] 
Convert a C unsigned long long to a Python integer object. Only available on platforms that support unsigned long long (or unsigned _int64 on Windows). 
n (int) [Py_ssize_t] 
Convert a C Py_ssize_t to a Python integer. 
c (bytes of length 1) [char] 
Convert a C int representing a byte to a Python bytes object of length 1. 
C (str of length 1) [int] 
Convert a C int representing a character to Python str object of length 1. 
d (float) [double] 
Convert a C double to a Python floating point number. 
f (float) [float] 
Convert a C float to a Python floating point number. 
D (complex) [Py_complex *] 
Convert a C Py_complex structure to a Python complex number. 
O (object) [PyObject *] 
Pass a Python object untouched (except for its reference count, which is incremented by one). If the object passed in is a NULL pointer, it is assumed that this was caused because the call producing the argument found an error and set an exception. Therefore, Py_BuildValue() will return NULL but won��t raise an exception. If no exception has been raised yet, SystemError is set. 
S (object) [PyObject *] 
Same as O. 
N (object) [PyObject *] 
Same as O, except it doesn��t increment the reference count on the object. Useful when the object is created by a call to an object constructor in the argument list. 
O& (object) [converter, anything] 
Convert anything to a Python object through a converter function. The function is called with anything (which should be compatible with void *) as its argument and should return a ��new�� Python object, or NULL if an error occurred. 
(items) (tuple) [matching-items] 
Convert a sequence of C values to a Python tuple with the same number of items. 
[items] (list) [matching-items] 
Convert a sequence of C values to a Python list with the same number of items. 
{items} (dict) [matching-items] 
Convert a sequence of C values to a Python dictionary. Each pair of consecutive C values adds one item to the dictionary, serving as key and value, respectively. 
If there is an error in the format string, the SystemError exception is set and NULL returned.
*/
void PyErr_SetString(PyObject *type, const char *message);
void PyErr_SetObject(PyObject *type, PyObject *value);
PyObject* PyErr_SetFromErrnoWithFilenameObject(PyObject *type, PyObject *filenameObject);
PyObject* PyErr_SetFromErrnoWithFilenameObjects(PyObject *type, PyObject *filenameObject, PyObject *filenameObject2);
PyObject* PyErr_SetFromErrnoWithFilename(PyObject *type, const char *filename);
PyObject* PyErr_SetFromWindowsErr(int ierr);
PyObject* PyErr_SetExcFromWindowsErr(PyObject *type, int ierr);
PyObject* Py_None;
//#Py_RETURN_NONE 
