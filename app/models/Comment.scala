package models

import play.api.db._
import play.api.Play.current
import anorm._
import anorm.SqlParser._
import java.util.Date
import play.core.Router
import javax.swing.text.DateFormatter
import java.util.regex.Pattern

case class Comment(id: Pk[Long], created: Long, post: Post, author: String, email: String, html: String)

object Comment {

  def findByPost(post: Post): Seq[Comment] = {
    DB.withConnection { implicit connection =>
      SQL("select * from comment where postId = {postId} order by created").on(
        'postId -> post.id).as({
          get[Pk[Long]]("id") ~
            get[Long]("created") ~
            get[String]("author") ~
            get[String]("email") ~
            get[String]("html") map {
              case id ~ created ~ author ~ email ~ html => Comment(id, created, post, author, email, html)
            }
        } *)
    }
  }

  def create(cmt: Comment): Unit = {
    DB.withConnection { implicit connection =>
      SQL("""
          insert into comment(id, created, postId, author, email, html) 
          values ({id}, {created}, {postId}, {author}, {email}, {html})
          """).on(
        'id -> cmt.id,
        'created -> new Date().getTime() / 1000,
        'postId -> cmt.post.id,
        'author -> cmt.author,
        'email -> cmt.email,
        'html -> cmt.html).executeUpdate()
    }
  }

  def generateWXR: String = {
    var wxr = """
    <?xml version="1.0" encoding="UTF-8"?>
    <rss version="2.0" xmlns:content="http://purl.org/rss/1.0/modules/content/"
    	xmlns:dsq="http://www.disqus.com/"
    	xmlns:dc="http://purl.org/dc/elements/1.1/"
    	xmlns:wp="http://wordpress.org/export/1.0/">
    <channel>
    """
    val sdf = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val pattern = Pattern.compile("<a href=\"(.+)\">(.+)</a>");
    DB.withConnection { implicit connection =>
      SQL("select * from comment").as({
        get[Pk[Long]]("id") ~
          get[Long]("created") ~
          get[String]("postId") ~
          get[String]("author") ~
          get[String]("email") ~
          get[String]("html") map {
            case id ~ created ~ postId ~ author ~ email ~ html => Comment(id, created, Post.findById(postId).get, author, email, html)
          }
      } *).foreach(comment =>
        wxr += """
        <item>
        <title>%s</title>
        <link>http://omsn.de/blog/%s</link>
        <content:encoded><![CDATA[%s]]></content:encoded>
        <dsq:thread_identifier>%s</dsq:thread_identifier>
        <wp:post_date_gmt>%s</wp:post_date_gmt>
        <wp:comment_status>open</wp:comment_status>
        <wp:comment>
        <wp:comment_id>%s</wp:comment_id>
        <wp:comment_author>%s</wp:comment_author>
        <wp:comment_author_email>%s</wp:comment_author_email>
        <wp:comment_author_IP>93.104.184.114</wp:comment_author_IP>
        <wp:comment_date_gmt>%s</wp:comment_date_gmt>
        <wp:comment_content><![CDATA[%s]]></wp:comment_content>
        <wp:comment_approved>1</wp:comment_approved>
        <wp:comment_parent>0</wp:comment_parent>
        </wp:comment>
        </item>
        """
          .format(
            "Re: " + comment.post.title,
            comment.post.id.get,
            "",
            comment.post.id.get,
            sdf.format(new java.util.Date(1000 * comment.post.created)),
            comment.id.get,
            {
              val matcher = pattern.matcher(comment.author)
              if (matcher.find())
                matcher.group(2)
              else
                comment.author
            },
            comment.email,
            sdf.format(new java.util.Date(1000 * comment.created)),
            comment.html))
    }
    wxr += """
    </channel>
    </rss>
    """
    wxr
  }

}
