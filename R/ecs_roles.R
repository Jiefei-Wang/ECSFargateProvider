
configTaskExecRoleArn <- function(x){
    if(!x$taskExecRoleArnVerified){
        roles <- aws.iam::list_roles(
            region = "us-east-1",
            key = aws.ecx::aws_get_access_key_id(),
            secret = aws.ecx::aws_get_secret_access_key())
        roleName <- vapply(roles, function(x)x$RoleName, character(1))
        roleArn <- vapply(roles, function(x)x$Arn, character(1))
        if(!is.null(x$taskExecRoleArn)){
            if(!x$taskExecRoleArn%in%roleArn){
                stop("The task execution role ARN <",x$taskExecRoleArn,"> does not exist")
            }
        }else{
            idx <- which(roleName=="ecsTaskExecutionRole")
            if(length(idx)){
                x$taskExecRoleArn <- roleArn[idx]
            }
        }
        x$taskExecRoleArnVerified <- TRUE
    }
    x$taskExecRoleArn
}
