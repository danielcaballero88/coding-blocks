import boto3


class S3:
    def __init__(self):
        self.client = self.get_client()

    @staticmethod
    def get_client():
        return boto3.client("s3")

    def download_file(
            self,
            s3_bucket_name: str,
            s3_file_path: str,
            output_file_path: str
        ):
        self.client.download_file(
            s3_bucket_name,
            s3_file_path,
            output_file_path,
        )
