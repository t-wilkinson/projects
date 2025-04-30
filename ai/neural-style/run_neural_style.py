import argparse
import neural_style
import os

if __name__ == '__main__':

    parser = argparse.ArgumentParser()
    parser.add_argument('--content-image', '-c', required=True)
    parser.add_argument('--style-image', '-s', required=True)
    parser.add_argument('--output-image', '-o', required=True)
    parser.add_argument('--token', '-t', required=True)
    args = parser.parse_args()

    neural_style.run(args.content_image, args.style_image, args.output_image)

    os.remove(args.content_image)
    os.remove(args.style_image)
