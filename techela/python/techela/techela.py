# This code can be put in any Python module, it does not require IPython
# itself to be running already.  It only creates the magics subclass but
# doesn't instantiate it yet.
from __future__ import print_function
from IPython.core.magic import (Magics, magics_class, line_magic)
from IPython.core.magic_arguments import argument, magic_arguments, parse_argstring
import subprocess, os

# The class MUST call this class decorator at creation time
@magics_class
class Techela(Magics):

    @magic_arguments()
    @argument('-g', '--get', type=str, help='get an assignment')
    @argument('-t', '--turn-in', type=str, help='Turn in an assignment')
    @argument('-u', '--update', type=str, help='Update an assignment')
    @line_magic
    def techela(self, line):
        'IPython magic function to get and turn in assignments'
        args = parse_argstring(self.techela, line)
        if args.get:
            print('Getting {}'.format(args.get))
            # git code here to pull it down
            self.get(args.get)
            
        elif args.turn_in:
            print('Turning in {}'.format(args.turn_in))
            # git code here to push it back
        elif args.update:
            print('Updating {}'.format(args.update))
            # git code to pull
            

    def get(self, label):
        # clone from a/label
        # change origin to src
        # add new remote as origin to s/label/userid=label
        
        userid = 'jkitchin' # We have to load this from somewhere.
        # We also need to load the course root directory from somewhere.
        
        my_env = os.environ.copy()
        # TODO this should be an executable script installed in the techela python module
        my_env['GIT_SSH'] = 'c:/Users/jkitchin/Dropbox/kitchingroup/jmax/techela/techela_ssh'
        
        # TODO cwd support is very fragile. Need to chdir, and compute absolute paths to avoid issues.
        os.chdir('C:\\Users\\jkitchin\\Dropbox\\kitchingroup\\jmax\\techela')
        
        if not os.path.exists(label):
            p = subprocess.Popen('git clone org-course@techela.cheme.cmu.edu:a/{}'.format(label).split(), env=my_env)
            print(p.communicate())
        
            # change remotes
            p = subprocess.Popen('git remote rename origin src'.split(),
                            cwd=label,stdout=subprocess.PIPE,stderr=subprocess.PIPE,
                            env=my_env)
            print(p.communicate())
        
            p = subprocess.Popen('git remote add origin org-course@techela.cheme.cmu.edu:s/{0}/{1}-{0}'.format(label, userid).split(),
                                    cwd=label,stdout=subprocess.PIPE,stderr=subprocess.PIPE,
                                env=my_env)
            print(p.communicate())
        else:
            # we have already cloned the repo so we open the assignment
            p = subprocess.Popen('ipython notebook --pylab=inline {0}/{0}.ipynb'.format(label).split(),
                                stdout=subprocess.PIPE,stderr=subprocess.PIPE)
            print(p.communicate())



def load_ipython_extension(ipython):
    # The `ipython` argument is the currently active `InteractiveShell`
    # instance, which can be used in any way. This allows you to register
    # new magics or aliases, for example.
    ip.register_magics(Techela)
    
# In order to actually use these magics, you must register them with a
# running IPython.  This code must be placed in a file that is loaded once
# IPython is up and running:
ip = get_ipython()
# You can register the class itself without instantiating it.  IPython will
# call the default constructor on it.
ip.register_magics(Techela)
