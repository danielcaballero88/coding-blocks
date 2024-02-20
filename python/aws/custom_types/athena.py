from typing import TypedDict
from datetime import datetime
from dateutil.tz import tzlocal


__workgroup_response_example = {
    "WorkGroup": {
        "Name": "ade",
        "State": "ENABLED",
        "Configuration": {
            "ResultConfiguration": {
                "OutputLocation": "s3://cdktoolkit-stagingbucket-58l2kih455z7/",
                "EncryptionConfiguration": {"EncryptionOption": "SSE_S3", "KmsKey": ""},
            },
            "EnforceWorkGroupConfiguration": False,
            "PublishCloudWatchMetricsEnabled": True,
            "RequesterPaysEnabled": False,
            "EngineVersion": {
                "SelectedEngineVersion": "AUTO",
                "EffectiveEngineVersion": "Athena engine version 3",
            },
        },
        "CreationTime": datetime(
            2022, 2, 23, 11, 26, 10, 427000, tzinfo=tzlocal()
        ),
    },
    "ResponseMetadata": {
        "RequestId": "b0ef4c30-c74c-4b8b-a1d8-2282b5476868",
        "HTTPStatusCode": 200,
        "HTTPHeaders": {
            "date": "Fri, 21 Jul 2023 12:47:39 GMT",
            "content-type": "application/x-amz-json-1.1",
            "content-length": "569",
            "connection": "keep-alive",
            "x-amzn-requestid": "b0ef4c30-c74c-4b8b-a1d8-2282b5476868",
        },
        "RetryAttempts": 0,
    },
}


class ResultConfiguration(TypedDict):
    OutputLocation: str
    EncryptionConfiguration: dict[str, str]


class Configuration(TypedDict):
    ResultConfiguration: ResultConfiguration
    EnforceWorkGroupConfiguration: bool
    PublishCloudWatchMetricsEnabled: bool
    RequesterPaysEnabled: bool
    EngineVersion: dict[str, str]


class WorkGroup(TypedDict):
    Name: str
    State: str
    Configuration: Configuration
    CreationTime: datetime


HTTPHeaders = TypedDict(
    "HTTPHeaders",
    {
        "date": str,
        "content-type": str,
        "content-length": str,
        "connection": str,
        "z-amzn-requestid": str,
    },
)


class ResponseMetadata(TypedDict):
    RequestId: str
    HTTPStatusCode: int
    HTTPHeaders: HTTPHeaders
    RetryAttempts: int


GetWorkGroupResponse = TypedDict(
    "GetWorkGroupResponse",
    {
        "WorkGroup": WorkGroup,
        "ResponseMetadata": ResponseMetadata,
    },
)


__start_query_execution_example = {
    "QueryExecutionId": "819fe841-f63a-4578-9c3c-2a3bb1609fa4",
    "ResponseMetadata": {
        "RequestId": "1ef961cf-da8a-4290-8e32-86dc6d9d4a0b",
        "HTTPStatusCode": 200,
        "HTTPHeaders": {
            "date": "Fri, 21 Jul 2023 12:29:50 GMT",
            "content-type": "application/x-amz-json-1.1",
            "content-length": "59",
            "connection": "keep-alive",
            "x-amzn-requestid": "1ef961cf-da8a-4290-8e32-86dc6d9d4a0b",
        },
        "RetryAttempts": 0,
    },
}


StartQueryExecutionResponse = TypedDict(
    "StartQueryExecutionResponse",
    {
        "QueryExecutionId": str,
        "ResponseMetadata": ResponseMetadata,
    }
)


ColumnInfoItem = TypedDict(
    "ColumnInfoItem",
    {
        "CatalogName": str,
        "SchemaName": str,
        "TableName": str,
        "Name": str,
        "Label": str,
        "Type": str,
        "Precision": int,
        "Scale": int,
        "Nullable": str,
        "CaseSensitive": bool,
    }
)


ResultSetMetadata = TypedDict(
    "ResultSetMetadata",
    {
        "ColumnInfo": list[ColumnInfoItem]
    }
)


ResultSet = TypedDict(
    "ResultSet",
    {
        "Rows": list,
        "ResultSetMetadata": ResultSetMetadata,
    }
)


GetQueryResultsResponse = TypedDict(
    "GetQueryResultsResponse",
    {
        "UpdateCount": int,
        "ResultSet": ResultSet,
        "ResponseMetadata": ResponseMetadata,
    }
)
