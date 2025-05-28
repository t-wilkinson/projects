# from mss import mss

# with mss() as sct:
# 	sct.shot()

# import mss
# # import mss.tools

# with mss.mss() as sct:
# 	monitor = {'top': 0, 'left': 0, 'width': 500, 'height': 500}
# 	output = 'sct-{top}x{left}x{width}x{height}.png'.format(**monitor)

# 	sct_img = sct.grab(monitor)

# 	mss.tools.to_png(sct_img.rgb, sct_img.size, output=output)
# 	print(output)

# import mss
from mss.linux import MSS as mss
import cv2
import numpy as np
import time


monitor = {'top': 40, 'left': 0, 'width': 800, 'height': 640}


def ss():
    with mss() as sct:
        img = np.asarray(sct.grab(monitor))
        gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        resized = resize(gray, size=(400, 200))
        cv2.imshow('title', resized)


def resize(img, size=(400, 200)):
    resized = cv2.resize(img, size, interpolation=cv2.INTER_AREA)
    return resized


def screen_record():
    # 800x600 windowed mode
    title = "[MSS] FPS benchmark"
    fps = 0
    last_time = time.time()

    with mss() as sct:
        while time.time() - last_time < 1:
            img = np.asarray(sct.grab(monitor))
            fps += 1

            cv2.imshow(title, img)
            if cv2.waitKey(25) & 0xFF == ord("q"):
                cv2.destroyAllWindows()
                break

    # sct.close()
    return fps


ss()
print(screen_record())
cv2.waitKey(0)
cv2.destroyAllWindows()
