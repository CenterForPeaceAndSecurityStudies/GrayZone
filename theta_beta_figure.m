%BEGIN ADD PARAMETERS
p_0=0.1;
k_c=0.3;
k_d_l=0.4;
k_d_h=0.6;
beta_c=0.5;
omega=0.15;
%beta_d=1.5

N=80;

theta_track=zeros(N,2);
beta_d=linspace(0.95,1.4,N)';
lb=1;
ub=2;
x0=(ub+lb)/2;


%beta_d=1
%theta=1.4
%testa=theta*p_0-k_c
%testb=theta*(p_0-(1/(4*beta_d))+k_d_l)


%CUT TO BOTTOM


%END CUT TO BOTTOM




%test=((theta*p_0-k_c - (theta*(p_0-(1/(4*beta_d)+k_d_l))-beta_c*((1/(4*beta_d) +k_d_l + k_c/theta)^2)))^2)

%sq_prep=[theta*p_0-k_c,0];
%sq_u=max(sq_prep);


for i=1:N;
    fun = @(theta) (max([theta*p_0-k_c,0]) - (theta*(p_0-(1/(4*beta_d(i)))+k_d_l)-beta_c*((1/(4*beta_d(i)) +k_d_l + k_c/theta)^2)))^2;
   %fun = @(theta) (sq_u - (theta*(p_0-(1/(4*beta_d(i)))+k_d_l)-beta_c*((1/(4*beta_d(i)) +k_d_l + k_c/theta)^2)))^2;
 %  fun = @(theta) -(theta*p_0-k_c -(theta*(p_0-1/(4*beta_d)+k_d_l)-beta_c*(1/(4*beta_d) +k_d_l + k_c/theta)^2))^2;
    A = [];
    b = [];
    Aeq = [];
    beq = [];
    [theta, val] = fmincon(fun,x0,A,b,Aeq,beq,lb,ub);
    theta_track(i,:)=[theta,val];
end;

%left of this prefer SQ, right prefer GZC; don't need last term, its just
%to make sure it's zeroing out
%theta_beta=[theta_track(:,1),beta_d,theta_track(:,2)]
theta_beta_sq_gz=[theta_track(:,1),beta_d]



for i=1:N;
   fun = @(theta) ((1-omega)*theta*(p_0-1/(4*beta_d(i))+k_d_h)+omega*(theta*p_0-k_c) - beta_c*(1/(4*beta_d(i))+k_d_h+k_c/theta)^2 - (theta*(p_0-(1/(4*beta_d(i)))+k_d_l)-beta_c*((1/(4*beta_d(i)) +k_d_l + k_c/theta)^2)))^2;
 %  fun = @(theta) -(theta*p_0-k_c -(theta*(p_0-1/(4*beta_d)+k_d_l)-beta_c*(1/(4*beta_d) +k_d_l + k_c/theta)^2))^2;
    A = [];
    b = [];
    Aeq = [];
    beq = [];
    [theta, val] = fmincon(fun,x0,A,b,Aeq,beq,lb,ub);
    theta_track(i,:)=[theta,val];
end;
theta_beta_gz_war=[theta_track(:,1),beta_d]


