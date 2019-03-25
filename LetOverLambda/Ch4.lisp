#|
a special type of macro, called read macro, opertes on the raw characters that make up your program.
The read macro is the device we use to process non-lisp syntax beforethe lisp reader get its paws on it.
Lisp gives you hooks for controlling every aspect of its behaviour. In other words, lisp lets you extend the reader, so that non-lisp objects are actually read in as lisp objects. Just as you build your application on top of lisp, extending it with macros and functions , lisp applications can, and frequently do, ooze out into this dimension of extensibility as well.
#. lets forms evaluated at read time, not when the form is evaluated.
* variable contains the value that resulted from evaluating the previous form, 
and the + variable contains that form.
because evaluation is finished after read,  (equal * (eval +)) is always T. However, if we we backquote and comma, this doesn't happen.
|#
