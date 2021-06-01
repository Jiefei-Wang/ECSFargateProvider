#' Initialize the Fargate provider
#'
#' Initialize the Fargate provider
#'
#' @inheritParams DockerParallel::initializeCloudProvider
#'
#' @return No return value
#' @export
setMethod("initializeCloudProvider", "ECSFargateProvider",
          function(provider, cluster, verbose){
              if(!provider$initialized){
                  if(!existCredentials()){
                      stop(
                          paste0("The AWS credentials do not exist, please set them via <aws.ecx::aws_set_credentials>\n",
                                 "The general information can be found at https://docs.aws.amazon.com/general/latest/gr/aws-sec-cred-types.html")
                      )
                  }
                  if(!cluster$isServerRunning()){
                      .setServerWorkerSameLAN(cluster, TRUE)
                  }
                  verbosePrint(verbose, "Initializing the ECS provider")
                  ## Cluster name
                  verbosePrint(verbose>1, "\tSetting up cluster")
                  clusterName <- configClusterName(provider, region = provider$region)
                  verbosePrint(verbose>1, "\tCluster name: \t", clusterName)
                  ## VPC
                  verbosePrint(verbose>1, "\tSetting up VPC")
                  VPCId <- configVpcId(provider, region = provider$region)
                  verbosePrint(verbose>1, "\tVPC: \t", VPCId)
                  ## subnet
                  verbosePrint(verbose>1, "\tSetting up subnet")
                  subnetId <- configSubnetId(provider, region = provider$region)
                  verbosePrint(verbose>1, "\tSubnet id: \t", subnetId)
                  ## gateway
                  verbosePrint(verbose>1, "\tSetting up gateway")
                  gatewayId <- configInternetGateway(provider, region = provider$region)
                  verbosePrint(verbose>1, "\tGateway: \t", gatewayId)
                  ## route table
                  verbosePrint(verbose>1, "\tSetting up route table")
                  routeTableId <- configRouteTable(provider, region = provider$region)
                  verbosePrint(verbose>1, "\tRoute table: \t", routeTableId)
                  ## route
                  verbosePrint(verbose>1, "\tSetting up default route")
                  configDefaultRoute(provider, region = provider$region)
                  verbosePrint(verbose>1, "\tDefault route finished")
                  ## security group
                  verbosePrint(verbose>1, "\tSetting up security group")
                  securityGroupId <- configSecurityGroup(provider, region = provider$region)
                  verbosePrint(verbose>1, "\tSecurity group: ",securityGroupId)
                  # Inbound permission
                  verbosePrint(verbose>1, "\tSetting up SSH and server-worker inbound permission")
                  port <- c(22, cluster@cloudConfig$serverPort)
                  ConfigInboundPermissions(x = provider, ports = port, region = provider$region)
                  verbosePrint(verbose>1, "\tInbound permission finished")

                  # Task execution role
                  verbosePrint(verbose>1, "\tSetting up task execution role")
                  roleArn <- configTaskExecRole(provider)
                  verbosePrint(verbose>1, "\tTask execution role:", roleArn)
                  # Task definition
                  verbosePrint(verbose>1, "\tSetting up task defintion")
                  configTaskDefinition(provider, cluster, region = provider$region)
                  verbosePrint(verbose>1, "\tTask defintion finished")
                  provider$initialized <- TRUE
              }
              invisible(NULL)
          })
#################################
##  Server management
#################################
#' Run the server container
#'
#' Run the server and return the server instance handle.
#' The function will set the environment variable `ECSFargateClusterStaticData`,
#' `ECSFargateClusterJobQueueName` to the container
#'
#' @inheritParams DockerParallel::runDockerServer
#'
#' @return The server handle in character
#' @export
setMethod("runDockerServer", "ECSFargateProvider",
          function(provider, cluster, container, hardware, verbose = 0L){
              stopifnot(length(provider$serverHandle)==0)
              verbosePrint(verbose>0, "Deploying server container")

              ## save the cloud config to the running instance
              encodedClusterStaticData <- encodeClusterStaticData(cluster)
              container <- container$copy()
              container$environment[["ECSFargateClusterStaticData"]] <- encodedClusterStaticData
              container$environment[["ECSFargateClusterJobQueueName"]] <- .getJobQueueName(cluster)
              fargateHardware <- getValidFargateHardware(hardware)
              if(verbose){
                  informUpgradedHardware(fargateHardware, hardware, 1)
              }
              taskDefName <- provider$serverTaskDefName
              instanceId <- ecsTaskScheduler(
                  provider = provider,
                  taskDefName=taskDefName,
                  container=container,
                  hardware= fargateHardware,
                  containerNum=1,
                  publicIpEnable=TRUE
              )

              if(is.null(instanceId)){
                  stop("Fail to deploy the ECS container, something is wrong")
              }
              provider$serverHandle <- instanceId

          })


#' Stop the server container
#'
#' stop the server container
#'
#' @inheritParams DockerParallel::stopDockerServer
#' @return No return value
#' @export
setMethod("stopDockerServer","ECSFargateProvider",
          function(provider, cluster, verbose){
              stopifnot(length(provider$serverHandle)==1)
              stopTasks(provider$clusterName, taskIds = provider$serverHandle)
          })

#' Get the server status
#'
#' Get the server status, return a character value which must be in one of three values
#' `"initializing"`, `"running"` or `"stopped"`.
#'
#' @inheritParams DockerParallel::getServerStatus
#' @returns Character(1)
#' @export
setMethod("getServerStatus", "ECSFargateProvider",
          function(provider, cluster, verbose){
              stopifnot(length(provider$serverHandle)<=1)
              if(length(provider$serverHandle)==1){
                  taskInfo <- getTaskDetails(provider$clusterName, taskIds = provider$serverHandle)
                  status <- tolower(taskInfo$status)
                  if(status %in% c("running", "stopped")){
                      status
                  }else{
                      "initializing"
                  }
              }else{
                  "stopped"
              }
          })

#' Get the server public/private IPs
#'
#' Get the server public/private IPs
#'
#' @inheritParams DockerParallel::getDockerServerIp
#'
#' @returns
#' A data.frame with publicIp and privateIp columns and each row corresponds to
#' an element in instanceHandles.
#' @export
setMethod("getDockerServerIp", "ECSFargateProvider",
          function(provider, cluster, verbose = 0L){
              stopifnot(length(provider$serverHandle) == 1)
              while(TRUE){
                  taskInfo <- getTaskDetails(provider$clusterName,
                                             taskIds = provider$serverHandle,
                                             getIP = TRUE)
                  if(taskInfo$publicIp!=""&&taskInfo$privateIp!=""){
                      break
                  }
                  if(taskInfo$status=="STOPPED"){
                      stop("The server has been stopped")
                  }
              }
              taskInfo$publicPort <- .getServerPort(cluster)
              taskInfo$privatePort <- .getServerPort(cluster)
              taskInfo
          }
)

#################################
##  worker management
#################################
#' Run the worker container
#'
#' Run the workers and return a character vector of worker handles.
#' The function will set the environment variable `ECSFargateClusterJobQueueName`,
#' `ECSFargateClusterServerSignature` and `ECSFargateClusterWorkerNumber` to the container
#'
#' @inheritParams ManagedCloudProvider::runDockerWorkerContainers
#'
#' @return A character vector of worker handles
#' @export
setMethod("runDockerWorkerContainers", "ECSFargateProvider",
          function(provider, cluster, container, hardware, workerNumber, verbose = 0L){
              verbosePrint(verbose>0, "Deploying worker container")
              instanceIds <- c()
              maxWorkers <- getMaxWorkerPerContainer(hardware)
              maxWorkers <- min(container$maxWorkerNum, maxWorkers)
              ## run the containers which have the maximum worker number
              containerWithMaxWorker <- floor(workerNumber/maxWorkers)
              if(containerWithMaxWorker>0){
                  instances <- ecsRunWorkers(
                      provider=provider,
                      cluster=cluster,
                      container=container,
                      hardware=hardware,
                      containerNumber=containerWithMaxWorker,
                      workerPerContainer=maxWorkers,
                      verbose=verbose
                  )
                  instanceIds <- c(instanceIds, instances)
                  if(length(instances) != containerWithMaxWorker){
                      stopTasks(provider$clusterName, instanceIds)
                      stop("Fail to deploy the ECS worker container, something is wrong")
                  }
              }
              ## Run the container which does not have the maximum worker number
              lastContainerWorkerNum <- workerNumber - maxWorkers*containerWithMaxWorker
              if(lastContainerWorkerNum!=0){
                  instance <- ecsRunWorkers(
                      provider=provider,
                      cluster=cluster,
                      container=container,
                      hardware=hardware,
                      containerNumber=1,
                      workerPerContainer=lastContainerWorkerNum,
                      verbose=verbose)
                  instanceIds <- c(instanceIds, instance)
                  if(length(instance)!=1){
                      stopTasks(provider$clusterName, instanceIds)
                      stop("Fail to deploy the ECS worker container, something is wrong")
                  }
              }

              ## Repeat the instance id worker number times.
              workerNumberPerContainer <- rep(maxWorkers, length(instanceIds))
              if(lastContainerWorkerNum!=0){
                  workerNumberPerContainer[length(instanceIds)] <- lastContainerWorkerNum
              }
              repeatVector(instanceIds, workerNumberPerContainer)
          }
)


#' Get the instance status
#'
#' Get the instance status
#'
#' @inheritParams ManagedCloudProvider::getDockerWorkerStatus
#'
#' @returns
#' A character vector with each element corresponding to a worker in workerHandles.
#' Each element must be one of three possible characters "initializing", "running" or "stopped"
#' @export
setMethod("getDockerWorkerStatus", "ECSFargateProvider",
          function(provider, cluster, workerHandles, verbose = 0L){
              taskInfo <- getTaskDetails(provider$clusterName, taskIds = workerHandles)
              instanceStatus <- rep("initializing", length(workerHandles))
              instanceStatus[taskInfo$status == "RUNNING"] <- "running"
              instanceStatus[taskInfo$status == "STOPPED"] <- "stopped"
              instanceStatus
          }
)

#' Kill the worker container
#'
#' Kill the worker container
#'
#' @inheritParams ManagedCloudProvider::killDockerWorkerContainers
#'
#' @return A logical vector indicating whether the killing operation is success for each instance
#' @export
setMethod("killDockerWorkerContainers", "ECSFargateProvider",
          function(provider, cluster, workerHandles, verbose = 0L){
              stopTasks(provider$clusterName, taskIds = workerHandles)
          }
)


#' Whether the cluster is running on the cloud?
#'
#' Whether the cluster is running on the cloud?
#' This function will check the instances in the ECS cluster defined by
#' `provider$clusterName` and find if there is any instance that has the same job queue
#' name as `.getJobQueueName(cluster)`
#'
#' @inheritParams DockerParallel::dockerClusterExists
#'
#' @return A logical value
#' @export
setMethod("dockerClusterExists", "ECSFargateProvider",
          function(provider, cluster, verbose = 0L){
              jobQueueName <- .getJobQueueName(cluster)
              clusterName <- provider$clusterName
              ## Check if the cluster exists
              clusterList <- listClusters()
              if(!clusterName%in%clusterList){
                  return(FALSE)
              }

              ## Check if the server exists
              serverEnv <- findServerEnvironment(
                  clusterName = clusterName,
                  jobQueueName = jobQueueName)
              !is.null(serverInfo)
          }
)


#' Reconnect the cluster
#'
#' Reconnect the cluster. This function will check the instances in
#' the ECS cluster defined by `provider$clusterName` and find if there is any
#' instance that has the same job queue name as `.getJobQueueName(cluster)`. If
#' so, the function can reconnect the cluster.
#'
#' @inheritParams DockerParallel::reconnectDockerCluster
#'
#' @return No return value
#' @export
setMethod("reconnectDockerCluster", "ECSFargateProvider",
          function(provider, cluster, verbose = 0L){
              cloudConfig <- .getCloudConfig(cluster)
              jobQueueName <- .getJobQueueName(cluster)
              clusterName <- provider$clusterName

              serverEnv <- findServerEnvironment(
                  clusterName = clusterName,
                  jobQueueName = jobQueueName)
              ## ask the user to select the server if multiple servers exist
              serverInfo <- selectServer(serverEnv)
              if(!is.null(serverInfo)){
                  ## Set the cloud config
                  provider$serverHandle <- serverInfo$serverHandle
                  clusterStaticData <- serverInfo$clusterStaticData
                  setDockerStaticData(cluster, provider, clusterStaticData)

                  ## Set the worker runtime
                  workerHandles <- findWorkerHandles(cluster)
                  addManagedWorkerHandles(cluster, workerHandles)
              }
          }
)



