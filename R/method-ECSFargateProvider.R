#' Create an ECS Fargate cloud provider
#'
#' Create an ECS Fargate cloud provider. Note that the arguments for the function
#' are the settings for ECS fargate. You can find them in the AWS ECS console.
#'
#' @param clusterName Character, the cluster name in ECS Fargate
#' @param serverTaskDefName,workerTaskDefName Character, the task defintion name for
#' the server and worker
#' @param securityGroupName Character, the security group name
#' @param vpcId,subnetId,securityGroupId,internetGatewayId,routeTableId The ID of
#' the network settings for the container
#' @param workerPublicIpEnable Logical, whether to enable the public IP for the worker.
#' If this value is `FALSE`, the worker will not be able to pull the container image from the
#' public network
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
                               workerPublicIpEnable = TRUE){
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
        workerPublicIpEnable=workerPublicIpEnable,
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
        initialized = FALSE
    )
}

.ECSFargateProvider$methods(
    show = function(){
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




