##From https://github.com/ranikay/shiny-reticulate-app/blob/8abc10d4bd8b1d0b88f49ed426c5b7dc2b0c9ea2/.Rprofile
##and https://github.com/rstudio/rsconnect/issues/359#issuecomment-638378795

# This file configures the virtualenv and Python paths differently depending on
# the environment the app is running in (local vs remote server).

# Edit this name if desired when starting a new app
VIRTUALENV_NAME = 'fill-batchalign-words'
PYTHON_PATH = '3.9.18'

# ------------------------- Settings (Edit local settings to match your system) -------------------------- #

if (Sys.info()[['user']] == 'shiny'){
  
  # Running on shinyapps.io
  Sys.setenv(PYTHON_PATH = PYTHON_PATH)
  # Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME) # Installs into default shiny virtualenvs dir
	# system("++++++++++reticulate::install_python()+++++++++++++")
	reticulate::install_python(version = PYTHON_PATH)
	# system("++++++++++pyenv install --list+++++++++++++")
	# system("pyenv install --list")
	# system("++++++++++THE REST+++++++++++++")
	# system("pip install git+https://github.com/AlejandroCiuba/elan_data@main")
	# reticulate::virtualenv_create(envname = VIRTUALENV_NAME, python = PYTHON_PATH)
	# reticulate::virtualenv_install(VIRTUALENV_NAME)
	# reticulate::use_virtualenv(VIRTUALENV_NAME, required = TRUE)
  Sys.setenv(RETICULATE_PYTHON = '/home/shiny/.pyenv/versions/3.9.18')
  
	
} else {
  
  # Running locally
  options(shiny.port = 7450)
  Sys.setenv(PYTHON_PATH = 'python3')
  Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME) # exclude '/' => installs into ~/.virtualenvs/
  # RETICULATE_PYTHON is not required locally, RStudio infers it based on the ~/.virtualenvs path
}
