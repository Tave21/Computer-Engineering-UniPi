//CORS filter implementation
package it.unipi.dsmt.FleetFra;

import jakarta.servlet.*;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.filter.GenericFilterBean;

import java.io.IOException;

//extends GenericFilterBean (from Spring) and implements Filter interface (from jakarta.servlet)
public class CORSFilter extends GenericFilterBean implements Filter {

    //doFilter method to handle the CORS requests, it is executed for each HTTP request through the filter
    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {
        //request: HTTP request from the client
        //response: HTTP response to the client
        //chain: FilterChain object to pass the request and response to the next filter in the chain or
        //to the servlet if the current filter is the last one in the chain

        HttpServletResponse httpResponse = (HttpServletResponse) response;
        //set the origin that can access the resources, * means all origins
        httpResponse.setHeader("Access-Control-Allow-Origin", "*");
        //set the allowed methods for the cross-origin requests
        //allowed methods are GET, POST, PUT, OPTIONS, DELETE
        httpResponse.setHeader("Access-Control-Allow-Methods", "*");
//        httpResponse.setHeader("Access-Control-Allow-Methods", "POST, GET, PUT, OPTIONS, DELETE");

        //set the allowed headers for the cross-origin requests
        httpResponse.setHeader("Access-Control-Allow-Headers", "*");
//        httpResponse.setHeader("Access-Control-Allow-Headers",
//                "Origin, X-Requested-With, Content-Type, Accept, X-Auth-Token, X-Csrf-Token, Authorization");

        //set the credentials flag to false, it means that the server does not accept credentials
        httpResponse.setHeader("Access-Control-Allow-Credentials", "false");
        //set the maximum age for the preflight requests
        httpResponse.setHeader("Access-Control-Max-Age", "3600");

        System.out.println("********** CORS Configuration Completed **********");

        chain.doFilter(request, response);
    }


}

