#!/bin/bash

# Now clone jmax
if [ ! -d "jmax" ]; then
  git clone https://github.com/jkitchin/jmax.git
fi

echo "jmax is installed. To use it, run this command in your terminal."
echo "emacs -q -l `pwd`/jmax/init.el"
echo "or"
echo "run the jmax.sh script created in this directory as ./jmax.sh in the terminal."

echo "#!/bin/bash
`pwd`/jmax/emacs/bin/runemacs.exe -q -l `pwd`/jmax/init.el
#end" > jmax.sh
chmod +x jmax.sh
#end
