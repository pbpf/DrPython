PyFunctionObject 
//The C structure used for functions.

PyTypeObject PyFunction_Type 
//This is an instance of PyTypeObject and represents the Python function type. It is exposed to Python programmers as types.FunctionType.

int PyFunction_Check(PyObject *o) 
//Return true if o is a function object (has type PyFunction_Type). The parameter must not be NULL.

PyObject* PyFunction_New(PyObject *code, PyObject *globals) 
//Return value: New reference.
//Return a new function object associated with the code object code. globals must be a dictionary with the global variables accessible to the function.

//The function’s docstring, name and __module__ are retrieved from the code object, the argument defaults and closure are set to NULL.

PyObject* PyFunction_NewWithQualName(PyObject *code, PyObject *globals, PyObject *qualname) 
//Return value: New reference.
//As PyFunction_New(), but also allows to set the function object’s __qualname__ attribute. qualname should be a unicode object or NULL; if NULL, the __qualname__ attribute is set to the same value as its __name__ attribute.

//New in version 3.3.

PyObject* PyFunction_GetCode(PyObject *op) 
//Return value: Borrowed reference.
//Return the code object associated with the function object op.

PyObject* PyFunction_GetGlobals(PyObject *op) 
//Return value: Borrowed reference.
//Return the globals dictionary associated with the function object op.

PyObject* PyFunction_GetModule(PyObject *op) 
//Return value: Borrowed reference.
//Return the __module__ attribute of the function object op. This is normally a string containing the module name, but can be set to any other object by Python code.

PyObject* PyFunction_GetDefaults(PyObject *op) 
//Return value: Borrowed reference.
//Return the argument default values of the function object op. This can be a tuple of arguments or NULL.

int PyFunction_SetDefaults(PyObject *op, PyObject *defaults) 
//Set the argument default values for the function object op. defaults must be Py_None or a tuple.

//Raises SystemError and returns -1 on failure.

PyObject* PyFunction_GetClosure(PyObject *op) 
//Return value: Borrowed reference.
//Return the closure associated with the function object op. This can be NULL or a tuple of cell objects.

int PyFunction_SetClosure(PyObject *op, PyObject *closure) 
//Set the closure associated with the function object op. closure must be Py_None or a tuple of cell objects.

//Raises SystemError and returns -1 on failure.

PyObject *PyFunction_GetAnnotations(PyObject *op) 
//Return the annotations of the function object op. This can be a mutable dictionary or NULL.

int PyFunction_SetAnnotations(PyObject *op, PyObject *annotations) 
//Set the annotations for the function object op. annotations must be a dictionary or Py_None.

//Raises SystemError and returns -1 on failure.
