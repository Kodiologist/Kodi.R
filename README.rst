Kodi.R is my .Rprofile, more or less. It's not a package because I fear that would be a maintenance nightmare.

Kodi.R has a lot of functions, and many of their names are short and cryptic. So, if you want to use some of these functions, I suggest you selectively import them::

    sys.source(getOption("Kodi.R.path"), Kodi <- new.env())
    for (name in c("qw", "ordf", "tversky.sid", "unpack.tversky"))
        assign(name, Kodi[[name]])

Put ``options(Kodi.R.path = "/path/to/Kodi.R)`` in your own .Rprofile.

License
============================================================

This program is copyright 2013 Kodi Arfer.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the `GNU General Public License`_ for more details.

.. _`GNU General Public License`: http://www.gnu.org/licenses/
