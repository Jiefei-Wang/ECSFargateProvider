#' Create an ECS Fargate cloud provider
#'
#' Create an ECS Fargate cloud provider. Note that the arguments for the function
#' are the settings for ECS fargate. You can find them in the AWS ECS console.
#'
#' @param clusterName Character, the cluster name in ECS Fargate
#' @param serverTaskDefName,workerTaskDefName Character, the task defintion name for
#' the server and worker
#' @param securityGroupName Character, the security group name
#' @param vpcId,subnetId,securityGroupId,internetGatewayId,routeTableId,taskExecRoleId The ID/ARN
#' of the resources for the container
#' @param enableWorkerPublicIp Logical, whether to enable the public IP for the worker.
#' If this value is `FALSE`, the worker will not be able to pull the container image from the
#' public network
#' @param logDriver Character, the log driver, if `logDriver = "auto"`, the awslogs will be used
#' @param logOptions Named list, the options for the log driver
#' @param region Character, the region of the ECS computing cluster
#' @export
ECSFargateProvider <- function(clusterName = "R-worker-cluster",
                               serverTaskDefName = "R-server-task-definition",
                               workerTaskDefName = "R-worker-task-definition",
                               securityGroupName = "R-parallel-security-group",
                               vpcId = NULL,
                               subnetId = NULL,
                               securityGroupId = NULL,
                               internetGatewayId = NULL,
                               routeTableId = NULL,
                               taskExecRoleId = NULL,
                               enableWorkerPublicIp = TRUE,
                               logDriver = c("auto", "none", "awslogs", "awsfirelens","splunk"),
                               logOptions = list(),
                               region = aws.ecx::aws_get_region()){
    stopifnot(!is.null(aws.ecx::aws_get_access_key_id()))
    stopifnot(!is.null(aws.ecx::aws_get_secret_access_key()))
    if(is.null(aws.ecx::aws_get_access_key_id())||
       is.null(aws.ecx::aws_get_secret_access_key())
    ){
        stop("No AWS credentials can be found, please set them via <aws.ecx::aws_set_credentials()>")
    }
    .ECSFargateProvider$new(
        clusterName = clusterName,
        serverTaskDefName = serverTaskDefName,
        workerTaskDefName = workerTaskDefName,
        securityGroupName = securityGroupName,
        vpcId = vpcId,
        subnetId = subnetId,
        securityGroupId = securityGroupId,
        internetGatewayId = internetGatewayId,
        routeTableId = routeTableId,
        enableWorkerPublicIp=enableWorkerPublicIp,
        taskExecRoleId = taskExecRoleId,
        clusterNameVerified = FALSE,
        serverTaskDefNameVerified = FALSE,
        workerTaskDefNameVerified = FALSE,
        securityGroupVerified = FALSE,
        vpcVerified = FALSE,
        subnetVerified = FALSE,
        internetGatewayVerified = FALSE,
        routeTableVerified = FALSE,
        routeVerified = FALSE,
        inboundPermissionVerified = FALSE,
        taskExecRoleVerified = FALSE,
        initialized = FALSE,
        logDriver = match.arg(logDriver),
        logOptions = logOptions,
        region = region
    )
}

.ECSFargateProvider$methods(
    show = function(){
        cat("Region:              ", .self$region, "\n")
        cat("Cluster name:        ", .self$clusterName, "\n")
        cat("Server task definition:     ", .self$serverTaskDefName, "\n")
        cat("Worker task definition:     ", .self$workerTaskDefName, "\n")
        cat("Security group name: ", .self$securityGroupName, "\n")

        if(!is.null(.self$vpcId)){
            cat("VPC ID:              ", .self$vpcId, "\n")
        }
        if(!is.null(.self$subnetId)){
            cat("Subnet ID:           ", .self$subnetId, "\n")
        }
        if(!is.null(.self$securityGroupId)){
            cat("Security group ID:   ", .self$securityGroupId, "\n")
        }
        if(!is.null(.self$internetGatewayId)){
            cat("Internet gateway ID: ", .self$internetGatewayId, "\n")
        }
        if(!is.null(.self$routeTableId)){
            cat("Route table ID:      ", .self$routeTableId, "\n")
        }
        invisible(NULL)
    }
)


