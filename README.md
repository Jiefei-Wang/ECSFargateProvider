Note: This vignette only teaches you how to create a provider, for the information on how to use `DockerParallel` package, please visit [CRAN:DockerParallel]

# Introduction
`ECSFargateProvider` provides [Amazon Elastic Container Service] to the `DockerParallel` package. It is able to deploy the container using the [Fargate] launch type. The provider can be created by

```r
provider  <- ECSFargateProvider()
provider
#> Region:               us-east-1 
#> Cluster name:         docker-parallel-cluster 
#> Server task definition:      R-server-task-definition 
#> Worker task definition:      R-worker-task-definition 
#> Security group name:  R-parallel-security-group 
#> VPC ID:                
#> Subnet ID:             
#> Security group ID:     
#> Internet gateway ID:   
#> Route table ID:
```
While many options are alterable by passing arguments to `ECSFargateProvider()`, the default value should be enough for creating a simple ECS provider. For more information, please see `?ECSFargateProvider`. 


# Credentials
For communicating with the cloud, you need to authenticate with the cloud provider. Amazon cloud uses `access key id` and `secret access key` to verify your identity. You can find the instruction on how to download your credentials from [AWS Documentation]. The ECS fargate provider uses `aws_set_credentials` from the package `aws.ecx` to find your credentials

```r
aws.ecx::aws_set_credentials()
#> $access_key_id
#> [1] "AK**************OYX3"
#> 
#> $secret_access_key
#> [1] "mL**********************************XGGH"
#> 
#> $region
#> [1] "us-east-1"
```
`aws_set_credentials` determines your credentials as well as the region of the cloud service. The region is the physical location of the cloud servers that will run your worker nodes. The function uses a variety of ways to find such information. The most important methods are as follow(sorted by the search order):

1. user-supplied values passed to the function

2. environment variables `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_DEFAULT_REGION`, and `AWS_SESSION_TOKEN`

You can either explicitly specify them or provide them as environment variables.



[CRAN:DockerParallel]: https://cran.r-project.org/web/packages/DockerParallel/index.html

[AWS Documentation]: https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html#Using_CreateAccessKey

[Amazon Elastic Container Service]: https://aws.amazon.com/ecs/

[Fargate]: https://aws.amazon.com/fargate/

