void PyErr_SetString(PyObject *type, const char *message);
void PyErr_SetObject(PyObject *type, PyObject *value);
PyObject* PyErr_SetFromErrnoWithFilenameObject(PyObject *type, PyObject *filenameObject);
PyObject* PyErr_SetFromErrnoWithFilenameObjects(PyObject *type, PyObject *filenameObject, PyObject *filenameObject2);
PyObject* PyErr_SetFromErrnoWithFilename(PyObject *type, const char *filename);
PyObject* PyErr_SetFromWindowsErr(int ierr);
PyObject* PyErr_SetExcFromWindowsErr(PyObject *type, int ierr);
PyObject* Py_None;