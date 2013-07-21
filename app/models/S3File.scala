package models

import java.io.File
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.model.PutObjectRequest
import com.amazonaws.services.s3.AmazonS3Client
import play.api.Play.current
import play.api.Play
import com.amazonaws.services.s3.model.CannedAccessControlList
import com.amazonaws.services.s3.model.StorageClass
import java.text.SimpleDateFormat
import java.util.TimeZone
import java.util.Date
import play.Logger

object S3File {

  val bucket = Play.current.configuration.getString("aws.s3.bucket").get
  val key = Play.current.configuration.getString("aws.s3.access.key").get
  val secret = Play.current.configuration.getString("aws.s3.secret.key").get
  val amazonS3 = new AmazonS3Client(new BasicAWSCredentials(key, secret))

  val sdf = new SimpleDateFormat("yyyy-MM-dd")
  sdf.setTimeZone(TimeZone.getTimeZone("UTC"))

  def create(file: File, fileName: String) = {
    val path = sdf.format(new Date()) + "/" + fileName
    Logger.debug("create: " + path)
    val req: PutObjectRequest = new PutObjectRequest(bucket, path, file)
      .withCannedAcl(CannedAccessControlList.PublicRead)
      .withStorageClass(StorageClass.ReducedRedundancy)
    val res = amazonS3.putObject(req); // upload file
    "upload/" + path
  }

  def getUrl(path: String): String = {
    "http://" + bucket + ".s3.amazonaws.com/" + path
  }
}