**NOTE:** This fork of https://github.com/python-babel/babel contains these changes:

* Fix duplicate locations when not writing line numbers to .po files
* Move detection of python-format to the python extractor
* Add support for detecting python-brace-format
* Add an extractor for Django templates with support for comments
* Rewritten wrapping algorithm that's closer to gettext's

About Babel
===========

Babel is a Python library that provides an integrated collection of
utilities that assist with internationalizing and localizing Python
applications (in particular web-based applications.)

Details can be found in the HTML files in the ``docs`` folder.

For more information please visit the Babel web site:

http://babel.pocoo.org/

Join the chat at https://gitter.im/python-babel/babel

Contributing to Babel
=====================

If you want to contribute code to Babel, please take a look at our
`CONTRIBUTING.md <https://github.com/python-babel/babel/blob/master/CONTRIBUTING.md>`__.

If you know your way around Babels codebase a bit and like to help
further, we would appreciate any help in reviewing pull requests. Please
contact us at https://gitter.im/python-babel/babel if you're interested!
