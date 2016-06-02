#!/bin/bash

# Now clone jmax
if [ ! -d "jmax" ]; then
  git clone https://github.com/jkitchin/jmax.git
fi

cd jmax
git submodule add https://github.com/jkitchin/emacs-win
git submodule update

echo "jmax is installed. To use it, run this command in your terminal."
echo "`pwd`/jmax/emacs-win/bin/runemacs.exe -q -l `pwd`/jmax/init.el"
echo "or"
echo "run the jmax.sh script created in this directory as ./jmax.sh in the terminal."

echo "#!/bin/bash
`pwd`/jmax/emacs-win/bin/runemacs.exe -q -l `pwd`/jmax/init.el
#end" > jmax.sh
chmod +x jmax.sh

echo "Opening jmax. The first time it will install a lot of packages. Be patient."
`pwd`/jmax/emacs-win/bin/runemacs.exe -q -l `pwd`/jmax/init.el
#end
