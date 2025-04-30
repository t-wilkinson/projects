import { MainHeader as Header } from 'Shared/Header'
import { MainFooter as Footer } from 'Shared/Footer'
import { ScrollView } from 'Shared/components'

export const Page = ({ navigation, children }) => (
  <ScrollView>
    <Header navigation={navigation} />
    {children}
    <Footer />
  </ScrollView>
)
