"""
This is a small module to load variables from a file
like if they were environment variables

We need to import load_dotenv from dotenv

Then use that function to load variables from a .env file
INTO the environment

Then we can use os.getenv to retrieve those variables from the environment
"""
from dotenv import load_dotenv
import os
from pathlib import Path

HERE = os.path.dirname(__file__)

# Take .env file
load_dotenv()
# Or a custom file (need absolute path)
env_path = os.path.join(HERE, "custom_env_file.env")
load_dotenv(dotenv_path=env_path)

# Then use os.getenv() to retrieve those variables from the environment
a = os.getenv("MY_ENVIRONMENT_VARIABLE")
passw = os.getenv("A_PASS")
email = os.getenv("EMAIL")