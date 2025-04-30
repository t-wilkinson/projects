import click

@click.command()
@click.option('--verbose', '-v', count=True, help='Set verbosity')
def test(verbose):
    click.echo(verbose)

if __name__ == '__main__':
    test()
