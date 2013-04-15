This is the offical repository for Appetizer, the application launcher, or dock, for Windows. It was originally an SVN repository, which explains the trunk/branches/tags structure.

# Building Appetizer

This project is developed and built using Visual C++ 2008 Express Edition and wxWidgets (2.8.9 Unicode).

## wxWidgets

wxWidget must be built in Unicode as a Multi-threaded (/MT option). [This page](http://wiki.wxwidgets.org/Microsoft_Visual_CPP_Guide) provides some very good information on how to build wxWidgets.

## Required environment variables

In order to build the project, you must set one `WXWIN` environment variable, which points to your wxWidgets folder (eg C:\wxWidgets-2.8.9). Follow [this link](http://priyank.co.in/readarticle.php?article_id=1) for some information on how to do that.

## Supported OS

Currently, Appetizer can only be built under Windows (tested under XP only). It also builds under Linux with a few tweaks but the interface will not render properly due to differences in the way wxImage and wxBitmap are handled. No tests have been done on Mac OS X although it is possible that it will work under this OS.

# More information

More information is available on the official website: [http://app.etizer.org/wiki](http://app.etizer.org/wiki)
