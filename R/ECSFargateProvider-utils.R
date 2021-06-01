ECSfilterList <- list(`tag:docker-parallel-tag`="docker-parallel-tag")
ECSTagTemplate <- list(
    list(ResourceType=NULL,
         Tag = list(
             list(Key= "docker-parallel-tag", Value = "docker-parallel-tag")
         )
    )
)

cleanupProvider <- function(x, verbose = TRUE){
    if(x$clusterNameVerified && x$clusterName=="R-worker-cluster"){
        verbosePrint(verbose, "Deleting worker cluster")
        tryCatch({
            deleteCluster(x$clusterName)
            x$clusterNameVerified <- FALSE
        },
        error = function(e) message(e))
    }

    if(x$vpcVerified){
        verbosePrint(verbose, "Deleting vpc")
        tryCatch({
            deleteVpc(x$vpcId)
            x$vpcVerified <- FALSE
            x$internetGatewayVerified <- FALSE
            x$securityGroupVerified <- FALSE
            x$subnetVerified <- FALSE
            x$routeTableVerified <- FALSE
        },
        error = function(e) message(e))
    }
    if(x$internetGatewayVerified){
        verbosePrint(verbose, "Deleting internet gateway")
        tryCatch({
            deleteInternetGateway(x$internetGatewayId)
            x$internetGatewayVerified <- FALSE
        },
        error = function(e) message(e))
    }
    invisible()
}


## base64 encode the cloud config setting
encodeClusterStaticData <- function(cluster){
    jsonlite::base64_enc(serialize(getDockerStaticData(cluster),NULL))
}
## base64 decode the cloud config setting
decodeClusterStaticData <- function(value){
    unserialize(jsonlite::base64_dec(value))
}

## Find the cloud config values from the environment variable
getContainerJobQueueName <- function(taskDescription){
    env <- taskDescription$overrides$containerOverrides[[1]]$environment
    env <- ArrayToList(env)
    jobQueueName <- env[["ECSFargateClusterJobQueueName"]]
    if(!is.null(jobQueueName)){
        jobQueueName
    }else{
        NULL
    }
}

getTaskEnvironment <- function(clusterName, taskIds){
    info <- ecs_describe_tasks(cluster = clusterName, tasks = taskIds)
    taskDefs <- info$tasks
    taskEnvs <- list()
    for(i in taskDefs){
        env <- i$overrides$containerOverrides[[1]]$environment
        env <- ArrayToList(env)
        taskEnvs[[i$taskArn]] <- env
    }
    taskEnvs
}

findServerEnvironment <- function(clusterName, jobQueueName){
    provider <- .getCloudProvider(cluster)
    jobQueueName <- .getJobQueueName(cluster)
    clusterName <- provider$clusterName

    serverHandles <- listRunningServers(cluster)
    if(!is.null(serverHandles)){
        info <- getTaskEnvironment(clusterName = clusterName, taskIds = serverHandles)
        idx <- c()
        for(i in seq_along(info)){
            id <- names(info)[i]
            env <- info[[i]]
            if(identical(env[["ECSFargateClusterJobQueueName"]], jobQueueName)){
                idx <- c(idx, i)
            }
        }
        info[idx]
    }else{
        NULL
    }
}


selectServer <- function(serverEnvironment){
    if(length(serverEnvironment) >= 1){
        if(length(serverEnvironment) > 1){
            answer <- menu(names(serverEnvironment),
                           title="Multiple servers exist on the cloud, please select:")
            if(answer == 0)
                answer <- 1
            serverHandle <- names(serverEnvironment)[answer]
            serverEnvironment <- serverEnvironment[[answer]]
        }else{
            serverHandle <- names(serverEnvironment)[1]
            serverEnvironment <- serverEnvironment[[1]]
        }

        encodedClusterData <- serverEnvironment[["ECSFargateClusterStaticData"]]
        stopifnot(!is.null(encodedClusterData))
        clusterStaticData <- decodeClusterStaticData(encodedClusterData)
        return(
            list(serverHandle = serverHandle,
                 clusterStaticData=clusterStaticData)
        )
    }else{
        NULL
    }
}



findWorkerHandles <- function(cluster){
    provider <- .getCloudProvider(cluster)
    clusterName <- provider$clusterName
    jobQueueName <- .getJobQueueName(cluster)
    serverSignature <-getServerSignature(cluster)

    workerHandles <- listRunningWorkers(provider)
    result <- c()
    if(!is.null(workerHandles)){
        info <- getTaskEnvironment(clusterName = clusterName, taskIds = workerHandles)
        for(i in seq_along(info)){
            id <- names(info)[i]
            env <- info[[i]]
            if(identical(env[["ECSFargateCloudJobQueueName"]], jobQueueName)&&
               identical(env[["ECSFargateCloudServerSignature"]], serverSignature)){
                workerNum <- as.numeric(env[["ECSFargateCloudWorkerNumber"]])
                result <- c(result, rep(id, workerNum))
            }
        }
    }
    result
}

listRunningServers <- function(cluster){
    provider <- .getCloudProvider(cluster)
    serverHandle <- listTasks(provider$clusterName, taskFamily = provider$serverTaskDefName)
    serverHandle
}

listRunningWorkers <- function(provider = NULL){
    clusterName <- provider$clusterName
    workerTaskDefName <- provider$workerTaskDefName
    workerHandles <- listTasks(clusterName, taskFamily = workerTaskDefName)
    workerHandles
}


## Repeat each x[i] element n[i] times
repeatVector <- function(x, n){
    unlist(lapply(seq_along(x), function(i) rep(x[[i]],n[i])))
}

getServerSignature <- function(cluster){
    provider <- .getCloudProvider(cluster)
    serverFromOtherSource <- .getServerFromOtherSource(cluster)
    if(serverFromOtherSource){
        publicIp <- .getServerPublicIp(cluster)
        privateIp <- .getServerPrivateIp(cluster)
        publicPort <- .getServerPublicPort(cluster)
        privatePort <- .getServerPrivatePort(cluster)
        queueName <- .getJobQueueName(cluster)
        serverSignature <- paste0(queueName,"-",
                                  publicIp,"-",
                                  privateIp,"-",
                                  publicPort,"-",
                                  privatePort)
    }else{
        serverSignature <- provider$serverHandle
    }
    serverSignature
}
