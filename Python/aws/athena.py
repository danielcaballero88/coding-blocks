import re
import time

import boto3

from custom_types.athena import GetWorkGroupResponse, StartQueryExecutionResponse, GetQueryResultsResponse
from s3 import S3

class Athena:
    def __init__(self):
        self.client = self.get_client()
        self.work_group_response = self.get_work_group()
        self.output_bucket_name = self.get_output_bucket_name()
        self.current_executing_queries = {}

    @staticmethod
    def get_client():
        return boto3.client("athena")

    def get_work_group(self) -> GetWorkGroupResponse:
        return self.client.get_work_group(WorkGroup="ade")

    @property
    def work_group_name(self) -> str:
        return self.work_group_response["WorkGroup"]["Name"]

    def get_output_bucket_name(self) -> str:
        bucket_path = self.work_group_response["WorkGroup"]["Configuration"]["ResultConfiguration"]["OutputLocation"]
        bucket_name = re.findall("[a-zA-Z0-9.\-_]{1,255}", bucket_path)[1]
        return bucket_name

    def get_query_results(self, query_execution_id: str) -> GetQueryResultsResponse:
        return self.client.get_query_results(QueryExecutionId=query_execution_id)

    def __start_query_execution(self, query: str) -> StartQueryExecutionResponse:
        response: StartQueryExecutionResponse = self.client.start_query_execution(
            QueryString=query,
            WorkGroup=self.work_group_name,
        )
        self.current_executing_queries[response["QueryExecutionId"]] = response
        return response

    def stop_all_executing_queries(self):
        for query_execution_id in self.current_executing_queries:
            self.client.stop_query_execution(QueryExecutionId=query_execution_id)

    def start_query_execution_and_wait(self, query_string: str, time_limit: int = 10) -> StartQueryExecutionResponse:
        print(f"Starting query execution: {query_string}")
        response = self.__start_query_execution(query_string)
        print(f"Query id: {response['QueryExecutionId']}")

        # Wait until query is complete
        print("Waiting for query completion...")
        start_time = time.time()
        while True:
            elapsed_time = time.time() - start_time
            print(f"Elapsed time: {elapsed_time} seconds.")
            if elapsed_time > time_limit:
                print(f"Cancelling query, surpassed the {time_limit} seconds limit.")
                self.client.stop_query_execution(
                    QueryExecutionId=response["QueryExecutionId"],
                )
                self.current_executing_queries.pop(response["QueryExecutionId"])
                raise Exception(f"Query excution time exceeded {time_limit}s limit.")
            try:
                self.get_query_results(response["QueryExecutionId"])
                break
            except Exception as exc:
                if "not yet finished" in str(exc):
                    time.sleep(0.1)
                else:
                    raise exc

        self.current_executing_queries.pop(response["QueryExecutionId"])
        return response

    def download_query_result_file(self, s3: S3, query_id: str, output_file_path: str):
        print(f"Downloading results of query_id={query_id} into {output_file_path}")
        s3.download_file(
            self.output_bucket_name,
            f"{query_id}.csv",
            output_file_path,
        )
        print("Done.")


if __name__ == "__main__":
    athena = Athena()
    athena.temp()
