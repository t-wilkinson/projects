import { MainHeader as Header } from 'shared/Header'
import { MainFooter as Footer } from 'shared/Footer'
import { ScrollView } from 'shared/components'

export const Page = ({ navigation, children }) => (
  <ScrollView>
    <Header navigation={navigation} />
    {children}
    <Footer />
  </ScrollView>
)
