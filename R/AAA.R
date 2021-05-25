#' The ECS fargate provider
#'
#' The ECS fargate provider, see `?ECSFargateProvider`
#'
#' @slot clusterName Character, the name of the ECS cluster
#' @slot initialized Logical, whether the provider has been initialized
.ECSFargateProvider <- setRefClass(
    "ECSFargateProvider",
    fields = list(
        clusterName = "character",
        serverTaskDefName = "character",
        workerTaskDefName = "character",
        securityGroupName = "character",
        vpcId = "character",
        subnetId = "character",
        securityGroupId = "character",
        internetGatewayId = "character",
        routeTableId = "character",
        taskExecRoleId = "character",
        enableWorkerPublicIp = "logical",
        clusterNameVerified = "logical",
        serverTaskDefNameVerified = "logical",
        workerTaskDefNameVerified = "logical",
        securityGroupVerified = "logical",
        vpcVerified = "logical",
        subnetVerified = "logical",
        internetGatewayVerified = "logical",
        routeTableVerified = "logical",
        routeVerified = "logical",
        inboundPermissionVerified = "logical",
        taskExecRoleVerified = "logical",
        initialized = "logical",
        logDriver = "character",
        logOptions = "list",
        serverHandle = "character",
        region = "character"
    ),
    contains = "ManagedCloudProvider"
)
