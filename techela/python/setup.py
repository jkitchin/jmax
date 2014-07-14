from distutils.core import setup
import os
  
setup(name = 'techela',
      version=0.1,
      description='python scripts to run a course using techela',
      url='http://github.com/jkitchin/techela',
      maintainer='John Kitchin',
      maintainer_email='jkitchin@andrew.cmu.edu',
      license='GPL',
      platforms=['linux'],
      packages=['techela'],
      scripts=['techela/techela_ssh'],
      long_description='''\
IPython magic functions for taking a techela course      ''')

# to push to pypi - python setup.py sdist upload
