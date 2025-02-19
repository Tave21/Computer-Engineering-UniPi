//Java Application based on Spring Boot

package it.unipi.dsmt.FleetFra;

//filter for managing Cross-Origin Resource Sharing (CORS) requests.
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
//Spring Boot class to run the application
public class FleetFraApplication {
    public static void main(String[] args) {
        SpringApplication.run(FleetFraApplication.class, args); }

    //custom CORS filter for the application
    @Bean
    public FilterRegistrationBean corsFilterRegistration() {
        FilterRegistrationBean registrationBean =
                new FilterRegistrationBean(new CORSFilter());
                //cors filter is implemented in the CORSFilter class
        registrationBean.setName("CORS Filter"); //set name to the filter
        registrationBean.addUrlPatterns("/*"); //set the URL pattern to the filter
                                                //apply the filter to all Application URLs
        registrationBean.setOrder(1); //set filter priority
        return registrationBean;
    }


}



