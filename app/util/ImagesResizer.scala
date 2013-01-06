package util

import java.awt.Image
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.io.File
import java.util.UUID
import play.Logger

object ImageResizer {

	def resize(source: File, newWidth: Int): File = {
		val newImage = ImageIO.read(source).getScaledInstance(newWidth, -1, Image.SCALE_SMOOTH)
		val bufferedImage = new BufferedImage(newImage.getWidth(null), newImage.getHeight(null), BufferedImage.TYPE_INT_RGB);
		bufferedImage.getGraphics().drawImage(newImage, 0, 0, null);
		var newFile = new File("/tmp/" + UUID.randomUUID() + ".jpg")
		ImageIO.write(bufferedImage, "jpg", newFile)
		newFile
	}

}