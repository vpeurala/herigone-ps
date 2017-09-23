provider "aws" {
  region = "eu-west-1"
}

module "vpc" {
    source = "github.com/terraform-community-modules/tf_aws_vpc"
    name = "herigone-ps-vpc"
    cidr = "10.0.0.0/16"
    public_subnets  = ["10.0.101.0/24", "10.0.102.0/24"]
    azs = ["eu-west-1a", "eu-west-1b"]
}
