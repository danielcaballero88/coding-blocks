This is a personal directory I make to learn to use python dotenv.
It should be necessary to create more secure apps.

What for:

Reads the key-value pair from .env file and adds them to environment variable. It is great for managing app settings during development and in production using 12-factor principles.

Usages:

The easiest and most common usage consists on calling load_dotenv when the application starts, which will load environment variables from a file named .env in the current directory or any of its parents or from the path specificied; after that, you can just call the environment-related method you need as provided by os.getenv.