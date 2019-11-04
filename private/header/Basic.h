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
  char         *ml_name£»    
  PyCFunction  ml_meth£»    
  int          ml_flags£»   
  char         ml_doc£»      
};


PyObject* Py_InitModule(char * name, PyMethodDef *);
PyObject* Py_BuildValue(const char *format, ...);
PyObject* Py_VaBuildValue(const char *format, va_list vargs); 


//#Py_RETURN_NONE 
