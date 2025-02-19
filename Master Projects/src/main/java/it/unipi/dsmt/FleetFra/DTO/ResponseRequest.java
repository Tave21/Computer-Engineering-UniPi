package it.unipi.dsmt.FleetFra.DTO;

public class ResponseRequest {

    private String answer;

    public ResponseRequest(){
    }

    public ResponseRequest(String answer, boolean isAdmin){
        this.answer = answer;
    }
    public ResponseRequest(String answer){
        this.answer = answer;
    }

    public String getAnswer(){
        return this.answer;
    }
    public void setAnswer(String answer){
        this.answer = answer;
    }
}
