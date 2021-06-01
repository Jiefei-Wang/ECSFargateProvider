#' List the cluster names, server or worker containers on ECS
#'
#' List the cluster names, server or worker containers on ECS. The `serverSignature` returned by
#' `ECSListWorkers` is the server id if the server if provided by the ECS cloud provider
#' or the server IP if the server if provided by the user.
#'
#' @param clusterName Character(1), the name of the cluster on ECS, you can call
#' `listClusters` to confirm the cluster names.
#'
#' @rdname clusterInfo
#' @returns
#' `ECSListClusters`: A character vector of the cluster names
#' `ECSListServers`: A data.frame with `id` and `jobQueueName` columns
#' `ECSListWorkers`: A data.frame with `id`, `jobQueueName`, `workerNumber` and `serverSignature` columns
#' @export
ECSListClusters <- function(){
    listClusters()
}


#' @rdname clusterInfo
#' @export
ECSListServers <- function(clusterName = "docker-parallel-cluster"){
    taskIds <- listTasks(clusterName = clusterName)
    taskEnvs <- getTaskEnvironment(clusterName = clusterName, taskIds = taskIds)

    res <- data.frame(id = character(), jobQueueName = character())
    for(i in seq_along(taskEnvs)){
        env <- taskEnvs[[i]]
        id <- ECSGetTaskShortArn(names(taskEnvs)[i])
        if("ECSFargateClusterStaticData"%in% names(env)&&
           "ECSFargateClusterJobQueueName"%in% names(env)){
            res[nrow(res)+1,] <- c(id, env[["ECSFargateClusterJobQueueName"]])
        }
    }
    res
}

#' @rdname clusterInfo
#' @export
ECSListWorkers <- function(clusterName = "docker-parallel-cluster"){
    taskIds <- listTasks(clusterName = clusterName)
    taskEnvs <- getTaskEnvironment(clusterName = clusterName, taskIds = taskIds)

    res <- data.frame(id = character(),
                      jobQueueName = character(),
                      workerNumber = character(),
                      serverSignature = character())
    for(i in seq_along(taskEnvs)){
        env <- taskEnvs[[i]]
        id <- ECSGetTaskShortArn(names(taskEnvs)[i])
        if("ECSFargateClusterJobQueueName"%in% names(env)&&
           "ECSFargateClusterServerSignature"%in% names(env)&&
           "ECSFargateClusterWorkerNumber"%in% names(env)){
            res[nrow(res)+1,] <- c(
                id,
                env[["ECSFargateClusterJobQueueName"]],
                as.integer(env[["ECSFargateClusterWorkerNumber"]]),
                ECSGetTaskShortArn(env[["ECSFargateClusterServerSignature"]])
            )
        }
    }
    res
}


